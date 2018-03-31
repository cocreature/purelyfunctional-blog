---
title: Calling Functions from Foreign C Libraries from a JITed LLVM Module
---

Sam Griffin recently [raised the
question](https://github.com/llvm-hs/llvm-hs/issues/193) of how to
call functions external functions, e.g., C functions defined in a
static or dynamic library, from an LLVM module that’s JITed using
`llvm-hs`. In this post, I will explain how you can accomplish this
for both static and dynamic libraries.

We start with a very simple C file `lib.c` that defines a function
called `external_function` which returns twice its argument. This is
the function that we will attempt to call from our LLVM module.

```c
#include <stdint.h>

int32_t external_function(int32_t x) {
    return 2 * x;
}
```

We can now compile this to an object file using `gcc -fPIC -c -o lib.o
lib.c`. (`-fPIC` is only necessary when we want to produce a dynamic
library but to keep things simple we will use the same object file for
building the static and the dynamic library in this post).

Creating a static library from this object file can be accomplished
using `ar rcs libexternalstatic.a lib.o`. The dynamic library can be
created using `gcc -shared -o libexternaldynamic.so lib.o`.

The LLVM module `module.ll` that we will be using declares
`external_function` and defines a function `f` which takes no argument
and returns the result of applying `external_function` to `21`.

```LLVM
; ModuleID = 'basic'
source_filename = "<string>"

declare i32 @external_function(i32)

define i32 @f() {
entry:
  %0 = call i32 @external_function(i32 21)
  ret i32 %0
}
```

Now that we have defined the module, we are ready to write the Haskell
code to JIT the module and then finally call the `f` function. For
this blogpost, we will declare the module using LLVM’s textual IR and
load it using `llvm-hs`’s `withModuleFromLLVMAssembly` but building
the module using `llvm-hs-pure`’s AST works as well.

There are two points that you need to pay attention to, if your JITed
module references external functions (for both static and dynamic
libraries):

1. Your resolver needs some way to find the symbol. We are going to
   use `getSymbolAdressInProcess` for this which is a function
   provided by `llvm-hs` that will search for loaded symbols in the
   current process.
2. `getSymbolAddressInProcess` will only find symbols in libraries
   that have been loaded before. This is accomplished by calling
   `loadLibraryPermanently` before you JIT the module. You can either
   pass the name of a dynamic library to `loadLibraryPermanently` or
   you can pass `Nothing` (equivalent to `dlopen(nullptr)`) which will
   load the symbols in the current process including the symbols in
   shared libraries that the executable is linked against.

This leaves us with the following resolver:
```haskell
resolver :: IRCompileLayer l -> SymbolResolver
resolver compileLayer =
  SymbolResolver
    (\s -> findSymbol compileLayer s True)
    (\s ->
       fmap
         (\a -> JITSymbol a (JITSymbolFlags False True))
         (getSymbolAddressInProcess s))
```         

In `main` we first call the aforementioned `loadLibraryPermanently`,
then we initialize the LLVM context, load the module, create the ORC
linking layer and the ORC compile layer, add the module to the compile
layer and finally we can search for the symbol `f` in our module and
call the function at that address using a [dynamic foreign
import](https://www.haskell.org/onlinereport/haskell2010/haskellch8.html#x15-1620008.5.1):

```haskell
main :: IO ()
main = do
  b <- loadLibraryPermanently Nothing
  unless (not b) (error "Couldn’t load library")
  withContext $ \ctx ->
    withModuleFromLLVMAssembly ctx (File "module.ll") $ \mod' ->
      withHostTargetMachine $ \tm ->
        withObjectLinkingLayer $ \objectLayer ->
          withIRCompileLayer objectLayer tm $ \compileLayer -> do
            withModule
              compileLayer
              mod'
              (resolver compileLayer) $
              \_ -> do
                mainSymbol <- mangleSymbol compileLayer "f"
                (JITSymbol mainFn _) <- findSymbol compileLayer mainSymbol True
                unless (mainFn /= WordPtr 0) (error "Couldn’t find JIT symbol")
                result <- mkFun (castPtrToFunPtr (wordPtrToPtr mainFn))
                print result
```

If you want to use the dynamic library, then all that’s left to do is
to add `extra-libraries: externaldynamic` to the executable section in
our cabal file. Depending on where you placed the shared library, you
will also have to set `extra-lib-dirs` to the directory containng the
library so that it is found at link time and the `LD_LIBRARY_PATH`
environment variable to make sure it is found when you run the
executable.

If you want to use the static library then things are a bit more
involved: Just adding `externalstatic` to `extra-libraries` will not
work since the linker will omit unused symbols when linking against
static libraries. Since the linker doesn’t know about the reference to
`external_function` in our JIT compiled module, this symbol will
thereby not end up in the binary. To fix this you need to use
`-Wl,--whole-archive,-lexternalstatic,--no-whole-archive` in the
`ld-options` section in your cabal file. This will force all symbols
in the `externalstatic` library to be included in the final executable
even if they are not referenced. We also need to ensure that the
symbols end up in the dynamic symbol table, since that is what
`getSymbolAddressInProcess` will look at. The corresponding in `GNU
ld` is called `--export-dynamic` but we use GHC’s `-rdynamic` option
here (by adding it to `ld-options`) which will use `--export-dynamic`
under the hood if you’re using GNU ld (but should at least in theory
also support other linkers). As for shared libraries, you might also
need to set `extra-lib-dirs` to make sure that the library is found at
link time. Since we are linking the library statically, there is no
need for messing with `LD_LIBRARY_PATH`. If you followed the steps,
you might have noticed that this still doesn’t quite work: You know
longer get symbol resolution errors but you will get a segfault.
Luckily this can be fixed by changing the relocation model of the
target machine to PIC instead of relying on the default set by
`withHostTargetMachine` which seems to be `Static` on X86. (I think
this has the effect of preventing LLVM from emitting call instructions
to immediates but I am not entirely sure why this is necessary. If you
do know more about this, I’d love here from you!).  The custom version
of `withHostTargetMachine` that sets the relocation model looks as
follows:

```haskell
withHostTargetMachine :: (TargetMachine -> IO a) -> IO a
withHostTargetMachine f = do
  initializeAllTargets
  triple <- getProcessTargetTriple
  cpu <- getHostCPUName
  features <- getHostCPUFeatures
  (target, _) <- lookupTarget Nothing triple
  withTargetOptions $ \options ->
    withTargetMachine target triple cpu features options Reloc.PIC CodeModel.Default CodeGenOpt.Default f
```

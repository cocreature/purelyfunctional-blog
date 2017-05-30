----
title: Dynamic loading of Haskell modules
----

Even though I don’t have any particular compelling use case for
dynamic loading of Haskell modules, it is something that I’ve been
wanting to do for quite some time. Sadly I have never been able to
produce anything but crashes so far. There is the
[plugins package](https://hackage.haskell.org/package/plugins) but I
have not gotten that to work either. The question seems to come up
from time to time, e.g. on
[reddit](https://www.reddit.com/r/haskell/comments/2z6ci1/dynamic_loadable_modules/),
but I have not seen an example that works so far. This morning I
decided to give it another shot and finally managed to get it to work!

Let us take the following module as an example:

```language-haskell
module Plugin(f) where
f :: String
f = "Monads are just monoids in the category of endofunctors, what’s the problem?"
```

We want to load the module in our main executable and print the string
`f`. The code is surprisingly simple and pretty much the same that is
also used in `plugins` and similar to the code used in `GHCi`. We
first need a function to create the `ELF` symbol name in our
executable from the package, module and `Haskell` symbol name.

```language-haskell
mangleSymbol :: Maybe String -> String -> String -> String
mangleSymbol pkg module' valsym =
  prefixUnderscore ++
  maybe "" (\p -> zEncodeString p ++ "_") pkg ++
  zEncodeString module' ++ "_" ++ zEncodeString valsym ++ "_closure"
```

For the details of `prefixUnderscore` take a look at the
[complete code](https://gist.github.com/cocreature/2e3ca5d921d08f8e0704b19b7dd186a6). `GHCi`
also has a similar function called
[`nameToCLabel`](https://downloads.haskell.org/~ghc/latest/docs/html/libraries/ghc/src/ByteCodeLink.html#nameToCLabel)
which can probably be used if you have a `Name` instead of dumb
strings.

To load our module we now only need to initialize the linker, load our
object file and lookup the symbol of the corresponding name.

```language-haskell
main :: IO ()
main =
  do initObjLinker
     loadObj "plugin.o"
     _ret <- resolveObjs
     ptr  <- lookupSymbol (mangleSymbol Nothing "Plugin" "f")
     case ptr of
       Nothing         -> putStrLn "Couldn’t load symbol"
       Just (Ptr addr) -> case addrToAny# addr of
                                 (# hval #) -> putStrLn hval
```

If you are confused by `(# hval #)` that’s just syntax for unboxed
tuples. Also note that this is not at all typesafe. It is up to you to
ensure that the symbol has the correct type.

We can now compile the plugin module using `ghc plugin.hs` and our
main module using `ghc -package ghc test.hs`. However if we run `./test` we get a cryptic error:

```language-none
test: plugin.o: unknown symbol `ghczmprim_GHCziCString_unpackCStringUtf8zh_closure'
zsh: segmentation fault (core dumped)  ./test
```

Why is this symbol not found, isn’t that a standard symbol that should
always be available? This is the point at which I gave up on my
previous tries.

Luckily GHC has a
[test](https://github.com/ghc/ghc/blob/master/testsuite/tests/rts/rdynamic.hs)
doing something similar (I have no idea why I have not found it
before). The solution is to simply compile our executable using `ghc
-package ghc -rdynamic test.hs`.

If we now run `test` we see the popular useful fact used to confuse
beginners (please don’t do that):

```language-none
Monads are just monoids in the category of endofunctors, what’s the problem?
```

You can change the text in `plugin.hs`, recompile it and rerun
`./test` (notably without recompiling `test.hs`) and it will show the
new text.

Since I’ve never used `rdynamic` before I did a bit of digging. The
reason for the error is actually independent of Haskell. It turns out
that there is a so called `dynamic symbol table` in an `ELF`
executable. Dynamically loaded code can only access symbols in that
table. However by default not every symbol in the executable is added
to the `dynamic symbol table`. Passing `rdynamic` tells the linker to
add all symbols to that table no matter if they’re used or not. That
way the dynamically loaded module has access to it.

You can also unload a modul using `unloadObj`. Thanks to Simon Marlow
the GC then
[unloads the object code](https://phabricator.haskell.org/rGHCbdfefb3b72a71cd0afca6e7766456c0d97c47c86).

Sadly I could only test this on Linux so I have no idea if it works on
Windows or OS X.

I hope this is useful for someone and look forward to see if and what
people use it for.

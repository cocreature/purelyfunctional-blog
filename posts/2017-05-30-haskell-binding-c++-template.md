---
title: Haskell bindings for template-heavy C++ code
---

This post decribes a technique for writing Haskell bindings (similar
tracks apply to other languages) to template-heavy C++ code when the
template instantiations that should be exposed are not statically
known. I am going to assume some rudamentary knowledge of C++
templates and the Haskell C FFI.

I originally faced this problem when trying to make the bindings to
ORC JIT in [llvm-hs](https://github.com/llvm-hs/llvm-hs) more
flexible, so the examples used in this post will be based on the API
of ORC JIT. The ORC JIT API is composed of various compile layers
which are responsible for compiling LLVM modules to object files (The
examples below call this method `compileModule`). There are base
layers which just compile modules directly but more importantly (for
this post), there are layers that wrap other layers and apply some
sort of transformation before passing the modified module to the
wrapped layer. Ignoring all the irrelevant details, we can imagine
that the C++-API for this looks as follows:

```language-cpp
class Module;
class Object;

// The base layer which compiles a module directly to object code. The details
// of how this is done are irrelevant for this post.
class BaseLayer {
    Object compileModule(Module module);
};

// A transform layer which first applies a function transforming the module
// before handing off compilation to the underlying base layer.
template <typename Base> class TransformLayer {
    TransformLayer(Base baseLayer, std::function<Module(Module)> transform)
        : baseLayer(std::move(baseLayer)), transform(std::move(transform)) {}
    Object compileModule(Module module) {
        Module transformedModule = transform(std::move(module));
        return baseLayer.compileModule(transformedModule);
    }
    std::function<Module(Module)> transform;
};

```

Being able to compose layers is great since it gives users a lot of
flexibility in how they want to build their JIT. However, it makes
providing Haskell bindings for that API tricky. Let’s first consider
what Haskell API we would like to end up with. It should expose the
same flexibility available in the C++ interface. In particular, users
should be able to choose which layers they want to use and how they
should be composed. A first attempt might look as follows:

```
import Foreign.C.Ptr

data Object
data Module
data BaseLayer
data TransformLayer baseLayer

newBaseLayer :: IO (Ptr BaseLayer)
newTransformLayer :: Ptr a -> IO (Ptr (TransformLayer a))
compileModule :: Ptr a -> Ptr Module -> IO (Ptr Object)
```

Some people might notice, that we are being to polymorphic here: We
are allowing pointers to arbitrary types to be used as compile
layers. We’ll come back to that later.

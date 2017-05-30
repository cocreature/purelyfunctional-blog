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
  public:
    Object *compileModule(Module *module);
};

// A transform layer which first applies a function transforming the module
// before handing off compilation to the underlying base layer.
template <typename BaseLayerT> class TransformLayer {
  public:
    TransformLayer(BaseLayerT &baseLayer,
                   std::function<Module *(Module *)> transform)
        : baseLayer(baseLayer), transform(std::move(transform)) {}
    Object *compileModule(Module *module) {
        Module *transformedModule = transform(module);
        return baseLayer.compileModule(transformedModule);
    }
    BaseLayerT &baseLayer;
    std::function<Module *(Module *)> transform;
};
```

Being able to compose layers is great since it gives users a lot of
flexibility in how they want to build their JIT. However, it makes
providing Haskell bindings for that API tricky. Let’s first consider
what Haskell API we would like to end up with. It should expose the
same flexibility available in the C++ interface. In particular, users
should be able to choose which layers they want to use and how they
should be composed. A first attempt at the low-level API might look as
follows:

```language-haskell
import Foreign.Ptr

data Object
data Module
data BaseLayer
data TransformLayer baseLayer

newBaseLayer :: IO (Ptr BaseLayer)
newTransformLayer :: Ptr a -> FunPtr (Ptr Module -> IO (Ptr Module)) -> IO (Ptr (TransformLayer a))
compileModule :: Ptr a -> Ptr Module -> IO (Ptr Object)
```

You might have noticed that we are being to polymorphic here: Users
shouldn’t be able to use pointers to arbitrary types to be used as
compile layers. We will come back to that later. 

Haskell does not support directly interfacing with C++, so we are
going to need to write a C wrapper to the C++ API. But we cannot write
a wrapper for `newTransformLayer`. C does not really have a concept of
polymorphism so we can’t write a function that accepts an arbitrary
layer. You might be tempted to just accept a `void*` and cast it and
hope for the best but even that will not work since calling the C++
constructor of `TransformLayer` requires statically knowing the type
of the base layer. Another non-solution would be to write different
wrappers for `newTransformLayer` for each type of base layers since
this contradicts our goal of exposing the full flexibility present in
the C++-API.

Let’s step back for a moment. What’s causing problems here is the fact
that C++ templates are a form of static polymorphism and we cannot
expose that via the C API. However, we can expose dynamic
polymorphism, i.e., virtual dispatch. So if LLVM would just have a
`CompileLayer` base class that `TransformLayer` and `BaseLayer`
inherit from, all would be good. So since LLVM does not provide this
base class, let’s just write it ourselves!

```language-cpp
class CompileLayer {
  public:
    virtual Object *compileModule(Module *module) = 0;
};
```

But `BaseLayer` and `TransformLayer` do not inherit this class. So we
are going to create a new class that wraps an arbitrary compile layer
and inherits from `CompileLayer`.

```language-cpp
template <typename T> class CompileLayerT : public CompileLayer {
  public:
    CompileLayerT(T layer) : layer(std::move(layer)) {}
    Object *compileModule(Module *module) override {
        return layer.compileModule(module);
    }
    T layer;
};
```

Now that we have the necessary machinery, we can write the
non-polymorphic C wrappers which we will use via the Haskell C FFI.

```language-cpp
extern "C" {
CompileLayer *newBaseLayer() {
    return new CompileLayerT<BaseLayer>(BaseLayer());
}
CompileLayer *newTransformLayer(CompileLayer *baseLayer,
                                Module *(*transform)(Module *)) {
    return new CompileLayerT<TransformLayer<CompileLayer>>(
        TransformLayer<CompileLayer>(*baseLayer, transform));
}
Object *compileModule(CompileLayer *layer, Module *module) {
    return layer->compileModule(module);
}
}
```

Finally, we are ready to get back to the Haskell code. Writing the
 bindings to the 3 C functions that we just defined is easy.

```language-haskell
{-# LANGUAGE ForeignFunctionInterface #-}
import Foreign.Ptr

data Object
data Module
data CompileLayer

foreign import ccall newBaseLayer ::
  IO (Ptr CompileLayer)
foreign import ccall newTransformLayer ::
  Ptr CompileLayer -> FunPtr (Ptr Module -> IO (Ptr Module)) -> IO (Ptr CompileLayer)
foreign import ccall compileModule ::
  Ptr CompileLayer -> Ptr Module -> IO (Ptr Object)
```

However, you have probably noticed that we have lost the separate
types for `BaseLayer` and `TransformLayer`. This is fine for the FFI
imports but we don’t want to present that API to the user. So we wrap
the above in a nicer Haskell API: We use a typeclass to represent
types which can be converted to a `Ptr CompileLayer` and add newtypes
for `BaseLayer` and `TransformLayer`. `TransformLayer` has a phantom
type parameter representing the base layer and our wrapper for
`newTransformLayer` ensures that it is correctly instantiated.

```language-haskell
foreign import ccall newBaseLayer ::
  IO (Ptr CompileLayer)
foreign import ccall newTransformLayer ::
  Ptr CompileLayer -> FunPtr (Ptr Module -> IO (Ptr Module)) -> IO (Ptr CompileLayer)
foreign import ccall compileModule ::
  Ptr CompileLayer -> Ptr Module -> IO (Ptr Object)

newtype BaseLayer = BaseLayer (Ptr CompileLayer)
newtype TransformLayer baseLayer = TransformLayer (Ptr CompileLayer)

class IsCompileLayer l where
  getCompileLayer :: l -> Ptr CompileLayer

instance IsCompileLayer BaseLayer where
  getCompileLayer (BaseLayer l) = l

instance IsCompileLayer (TransformLayer l) where
  getCompileLayer (TransformLayer l) = l

newBaseLayer' :: IO BaseLayer
newBaseLayer' = BaseLayer <$> newBaseLayer

newTransformLayer' :: IsCompileLayer l => l -> FunPtr (Ptr Module -> IO (Ptr Module)) -> IO (TransformLayer l)
newTransformLayer' baseLayer transform =
  TransformLayer <$> newTransformLayer (getCompileLayer baseLayer) transform

compileModule' :: IsCompileLayer l => l -> Ptr Module -> IO (Ptr Object)
compileModule' layer module' =
  compileModule (getCompileLayer layer) module'
```

### Caveats

1. I’ve only shown constructors to this API. Usually you also want to
   add destructors which free the allocated layers, otherwise you are
   going to leak memory. Luckily you can mark the destructor of
   `CompileLayer` as `virtual` and can then use the same C wrapper for
   all layers.
  
2. Turning static polymorphism into dynamic polymorphism does incurr a
   slight performance cost. In this case, this is probably irrelevant
   but if you are wrapping a template function that wraps “small”
   types, e.g., a function that accepts different types of integers
   and performs cheap operations on them it might matter.

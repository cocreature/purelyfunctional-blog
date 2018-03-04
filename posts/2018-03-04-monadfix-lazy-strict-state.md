---
title: MonadFix and the Lazy and Strict State Monad
---

In this post, I will assume rudamentary familiarity with the different
`Monad` instances of the lazy and strict state monad and
`MonadFix`. If you are not familiar with these concepts or want to
brush up your knowledge, I recommend Kwang Yul Seo’s [post on the lazy
and strict state
monad](https://kseo.github.io/posts/2016-12-28-lazy-vs-strict-state-monad.html)
and Will Fancher’s [post on
`MonadFix`](https://elvishjerricco.github.io/2017/08/22/monadfix-is-time-travel.html).

Recently, `llvm-hs-pure` got a new API for building modules called
`IRBuilder` which makes this process significantly more convenient by
taking care of a lot of the necessary book keeping. In particular, the
API is built upon a state monad that tracks variables and creates
fresh variables as necessary, allows the use of monadic binds to refer
to operators and more. In the context of LLVM references to variables
or blocks often end up being circular, e.g., the branch instructions
in the basic blocks in a loop will form a cycle referencing each
other. While monadic binds can’t be recursive by default, `MonadFix`
and the `RecursiveDo` extension lift this restriction and thereby
allow for a very convenient API even in the presence of recursive
definitions. For a more detailed blogpost on a very similar API, I
recommend Lewis’ [post on the ASM
monad](http://wall.org/~lewis/2013/10/15/asm-monad.html).

Recursive functions are another case where references end up being
circular and thereby require `MonadFix`. Sadly, this usecase was
completely broken in `llvm-hs-pure` as Pavol Klacansky noticed in a
[bugreport](https://github.com/llvm-hs/llvm-hs/issues/181): All
attempts to build modules this way led to an infinite loop and GHC’s
infamous `<<loop>>` exception. After investigating this problem, I
figured out that replacing the strict state monad by the lazy state
monad solved the problem and lead to the expected behavior instead of
an infinite loop. In the following, I’m going to present a simplified
version of the problem and explain why the two versions differ.

We’ll start out by defining a very simple type representing the
instructions in our program. For this example, we only need to instructions:

1. A `Dummy` instruction and
2. a `Reference` instruction that refers to the result of another instruction by its name.

```haskell
data Instr
  = Reference String
  | Dummy
  deriving Show
```

We can now define the `Builder` monad which is used to build the list
of instructions. `Builder` is just a type synonym for a `State` monad
with the state being a list of `(String, Instr)` pairs. We’ll also
define `runBuilder` function that run a builder with an initial state
consisting of an empty list of instructions and returns the final
list.

```haskell
type Builder a = State [(String, Instr)] a

runBuilder :: Builder a -> [(String, Instr)]
runBuilder a = execState a []
```

Emitting an instruction appends it to the list of instructions and
returns the name of the instruction. We also define two convenience
wrappers for emitting `Dummy` and `Reference` instructions.

```haskell
emitInstr :: (String, Instr) -> Builder String
emitInstr (n, i) = do
  modify (\instrs -> instrs ++ [(n, i)])
  pure n

dummy :: String -> Builder String
dummy n = emitInstr (n, Dummy)

reference :: String -> String -> Builder String
reference n ref = do
  let instr = Reference ref
  emitInstr (n, instr)
```

Finally, we can define a very simple example program consisting of a
`Reference` instruction and a `Dummy` instruction with the `Reference`
instruction referencing the `Dummy` instruction which is defined
*later* (that is why we need `MonadFix` and `RecursiveDo` here).

```haskell
example :: Builder ()
example = mdo
  ref <- reference "ref" foo
  foo <- dummy "foo"
  pure ()
```

You can use the following definition for `main` to test this example.

```haskell
main :: IO ()
main = print (runBuilder example)
```

This example will work with both the lazy and the strict state monad.
However, if we change the definition of `reference` as shown below,
running the example will result in an infinite loop.

```haskell
reference :: String -> String -> Builder String
reference n ref = do
  let instr = Reference ref
  case ref of
    !a -> emitInstr (n, instr)
```

Introducing the strict pattern match here might seem silly and in this
isolated example it definitely is. However, in general it is
definitely possible that the way an instruction is emitted depends on
the reference and thereby requires a pattern match. In `llvm-hs`, the
`call` instruction [checks if the callee has a void return
type](https://github.com/llvm-hs/llvm-hs/blob/944ce3849b773137e4704c23e9fef715e2c8599d/llvm-hs-pure/src/LLVM/IRBuilder/Instruction.hs#L180)
which resulted in the issue mentioned above. To better understand why
the strict and the lazy monad behave differently here, I am going to
substitute the `Monad` and `MonadFix` instances and inline the
definitions.

Let us start by removing the use of `mdo` and replace it by an explicit use of `mfix`.

```haskell
example = do
  mfix $ \foo -> do
    ref <- reference "ref" foo
    foo' <- dummy "foo"
    pure foo'
  pure ()
```

Next, we can substitute the definition of `mfix`. Since `State s a` in
`transformers` is defined as a `StateT s Identity a`, the definition
can look a bit complicated. For this post, we are going to assume that
`State` has not been defined as a transformer and provide definitions
for this simplified version of `State`. You can see the recursion in
`mfix` by `a` occuring both on the left and on the right of `=`.

```haskell
newtype State s a = State { runState :: s -> (a, s) }
mfix f = State (\s -> let (a, s') = runState (f a) s in (a, s'))
```

In the next step, we inline this definition of `mfix`.

```haskell
example :: State [(String, Instr)] ()
example = do
  State $ \s ->
    let (foo, s') =
          runState (do ref <- reference "ref" foo
                       foo' <- dummy "foo"
                       pure foo'
                   )
                   s
    in (foo, s')
  pure ()
```

Finally, we desugar `do` notation and inline `reference`, `dummy` and `runState`.

```haskell
example :: State [(String, Instr)] ()
example = do
  State $ \s ->
    let (foo, s') =
          let (ref, s'') = 
                case foo of !a -> ("ref", s ++ [("ref", Reference foo)])
              (foo', s''') = ("foo", s'' ++ [("foo", Dummy)])
          in (foo', s''')
    in (foo, s')
  pure ()
```

The above definition uses the bind implementation of the lazy state
monad, for the strict state monad, we need to change the let statement
to be strict in the tuple (note that pattern matches in `let` statements are lazy by default):

```haskell
let !(ref, s'') = 
      case foo of !a -> ("ref", s ++ [("ref", Reference foo)])
    (foo', s''') = ("foo", s'' ++ [("foo", Dummy)])
in (foo', s''')
```

At this point, the difference becomes clear: For the strict state
monad, forcing `(foo, s')` forces `(ref, s'')` which in turn ends up
forcing `foo` which has not yet been computed so we run into an
infinite loop. For the lazy state monad, the evaluation of the `(ref,
s'')` tuple and thereby also the case statement on `foo` is lazy and
thus we can first evaluate that `foo = "foo"` before evaluating the
`case` statement and avoid the infinite loop.

### Conclusion

When asked what the lazy state monad is for, the most common response
is infinite states as demonstrated by Kwang in the
[post](https://kseo.github.io/posts/2016-12-28-lazy-vs-strict-state-monad.html)
mentioned at the beginning of this post. In this article, we have seen
a different usecase in combination with `MonadFix` where monadic
actions depend on recursive bindings and the lazy state monad prevents
an infinite loop.

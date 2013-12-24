### Implication instead of parents for type classes

Right now we're storing the parents of type classes, so that when we're
type checking we have a tree up which we can walk until we either find the
parent, or report an error. However, this model only works when we have a
single type variable in the type class, like `Functor` or `Show`. How
about when there are multiples?

```
typeclass VarMonad (m :~ Monad) v =
  sig new : a -> m (v a);
  sig get : v a -> m a;
  sig put : v a -> a -> m ();

instance VarMonad IO IORef = ...
instance VarMonad (ST s) (STRef s) = ...
```

`VarMonad` is parameterized by TWO type variables; the first of which
is a monad and the second is a variable. It seems the order of these guys
is irrelevant, that it's just a set of types. But let's see what's going
on here, first of all what we need to store in the type class:

```
data TypeClass = TC Name [Type] [Expr]
```

Note we're ignoring functional dependencies for now.

So for example `VarMonad` would be represented:

```
TypeClass
  "VarMonad"
  [(TVar ["Monad"] "m"), (TVar [] "v")]
  [ Sig "new" (..)
  , Sig "get" (....)
  , Sig "put" (...)]
```

This info, we could just get from a parse. We need also to be able to get
the kinds of each of these. In this case, that's `Type -> Type` for `m`
and `Type` for `v`.

That suggests a second form for a type class record. This one doesn't
need a name because its name will be the key by which this record is
found.

```
TC
  [(TVar ["Monad"] "m", type_ :-> type_), (TVar [] "v", type_)]
  [ Sig "new" (..)
  , Sig "get" (....)
  , Sig "put" (...)]
```

So then really we've just added kinds... do we need a whole new type for that?
Well it's OK because we don't want to be tied to the AST anyway, so whatevs.

Alright we're going to downgrade a bit to single-parameter type classes for now.
We can build up in a bit.

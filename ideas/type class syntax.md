```
typeclass Functor (f : type -> type) =
  sig map : (a -> b) -> f a -> f b;
;

typeclass Applicative (f :~ Functor) =
  sig pure : a -> f a;
  sig <*>  : f (a -> b) -> f a -> f b;
;

typeclass Monad (m :~ Applicative) =
  sig return : a -> m a;
  sig >>=    : m a -> (a -> m b) -> m b;
;
```

in effect, `:~` can be seen as a kind of type annotation for types, a 'kind annotation' if you will. Except it can have multiple kinds/types.

```
typeclass Eq (a : type) =
  sig == : a -> a -> Bool;
;

typeclass Ord (a :~ Eq) =
  sig compare : a -> a -> Ordering;
;

typeclass Show (a : type) =
  sig show : a -> String;
;

typeclass MonadState s (m :~ Monad) =
  sig get : m s;
  sig put : s -> m ();
;
```

Instance declaration syntax

```
instance Functor [] =
  let map f =
    let map' acc [] = acc
    |   map' acc (x::xs) = map' (f x::acc) xs;
    map' [] ~> reverse;
;

instance MonadState s (StateT s (m :~ Monad)) =
  let get = get;
  let put = put;
  let state = state;
;
```

It's probably better to have a list of type variables in a class instead of just a list of names. That way we can include type class restrictions. We might also want a way to annotate kinds, in case we want to make it explicit.

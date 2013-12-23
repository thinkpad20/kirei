# Thoughts on Type Classes

Type classes come into play with unification. For example, the following unification is allowed:

```
All a. a -> String =>
  Show a. a -> String
```

And so is this:

```
All a. a -> String =>
  Show a. a -> String =>
    String -> String
```

So is this:

```
All a f. a -> f a =>
  All a, Functor f. a -> f a =>
    All a, Monad f => a -> f a =>
      Monad f. Number -> f Number =>
        Number -> [Number]
```

So we're progressively refining the restrictions on each free type variable
in the polytype. A restriction to a type class can only be made if the type
class is a subclass of the previous restriction. A restriction to a type
can only be made if the type implements the type class. We can think of `All`
as the base type for all type classes, and that every type has an implicit
implementation of `All` so that it can always be derived from. This changes
our picture of polytypes: Previously we had polytypes with sets of free
variables, and the only two options for a variable were that it was free, or
not. Now, this suggests something like this:

```
data Polytype = Polytype [(TypeClass, Name)] Type
```

Then when we make applications, we need to look at the type classes. For
example, let's assume

```
foo: All a. a -> Number;
bar: String;
```

Then when we apply `foo` to `bar`, we would unify `a -> Number` with
`String -> b`, giving the substitutions `{a => String`, `b => Number}` and
returning the type `Number`
Now, let's look at an Applicative:

```
pure: All a, Applicative f => a -> f a;
<*> : All a b, Applicative f => f (a -> b) -> f a -> f b;


let foo: [Number -> Number] = map (*) [1..10];
```

Now when we apply `pure` to `3`, we unify `Applicative f => a -> f a` with
`Number -> b`, giving `{a => Number, b => f a}, and returning the type
`Applicative f. f Number`.
Now let's try `foo <*> pure 3` to that. First we find the type of `foo <*>`: we unify
`(type of <*>)` with `(type of foo -> newtype)`, which means unify
`Applicative f. f (a->b) -> f a -> f b` with `[] (Number -> Number) -> c`,
which produces the substitutions `{f => [], a => Number, b => Number, c => f a -> f b}`.
After applying those substitutions, we have `foo <*> : [Number] -> [Number]`. Next,
we apply that to `pure 3`, which we had previously determined was
`Applicative f. f Number`. This means we unify `[Number] -> [Number]` with
`f Number -> d`. That produces the substitutions `f => [], d => [Number]`,
giving the final type as `[Number]`, which is exactly what we had expected! We didn't
have to do anything differently! The only real modification we should make,
*it seems*, is that when we make a substitution like `f => []`, we need to
verify that that is a valid substitution with respect to the type class hierarchy.
We also need to be able to distinguish between a "plain" type variable, and one
decorated with some type class - suggesting augmenting the `Type` type with a
type class constructor:

```
data Type = TVar Name
          | ...
          | TClass Name Type
```

Note in the above example, we instantiated `pure` without its `All` restriction,
but *with* its `Applicative` restriction. Not sure if this is the right way to go.

# Union types, and unifying based only on type class:

Ex: `[1, "hello"]` is illegal under HM, but what if we created a type
union, `U = (Number | String)`?
then `[1, "hello"] : [U]`

Alternatively, we could unify those based on type class:
`[1, "hello"] : [(Show, Ord, Eq, ...)]`

In other words, for a list of types `a1, a2, ... aN`, we can type it as
`[classes(a1) ∩ classes(a2) ∩ ... ∩ classes(aN)]`

Let's look a little closer at how that might work, with `[1, "hello"]`:

In AST form, `[1, 'hello']` looks like

```
Apply
  (Apply
    (TypeName "::")
    (Number 1.0))
  (Apply
    (Apply
      (TypeName "::")
      (String "hello"))
    (TypeName "[]"))
```

From the type checker point of view, the first thing that will happen is
instantiating `[]` to `a -> [a] -> [a]`.

The application of this to `1` gives unification `{a => Number}` and a return
type of `[Number] -> [Number]`.

The next thing that would happen is application of `::` to `"hello"`, which
gives `[String] -> [String]`, and since we apply that to `[]`, we have a
`[String]`. So in the end we have `[Number] -> [Number]` applied to `[String]`.
This means we'll try to `unify ([Number] -> [Number]), ([String] -> a)`, which
would try to unify `Number` with `String` and throw an error. This is a tough
nut to crack, because now we need to decide if we simply allow creating a
union type, if we look at the type classes each supports, if we only allow this
because it's `::`, if we allow it for all polymorphic functions, or...?

And what about applied types? E.g., `Maybe`? This could be shitty...


Let's imagine that we're applying `bar: Applicative a. a Number -> a Number`,
and `foo : Functor f. f Number`. then imagine we are writing

```
bar foo

--> unify ((a :~ Applicative) Number -> a Number) ((f :~ Functor) -> b)
  --> unify ((a :~ Applicative) Number) ((f :~ Functor) Number)
    --> unify (a :~ Applicative) (f :~ Functor)
      --> getParents "Functor"
        --> {"Functor"}
```

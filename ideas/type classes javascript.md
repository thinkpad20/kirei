## Last part of type classes... getting it to work in javascript!

### What we need:

* When compiling a type class declaration:
  - record that these methods are overloaded
* When compiling an instance for a type:
  - create a name where the method and the type are appended
  - compile the body of the function into one with that name
* When compiling a function call:
  - check if we are calling an overloaded function
  - in that case:
    * figure out which function we are actually referring to, using the types
    * call that function.

### Example:

```
typeclass Functor f =
  sig map : (a -> b) -> f a -> f b;
;

instance Functor Maybe =
  let map f m = case m of
    Nothing -> Nothing
  | Just x  -> Just (f x);
;

instance Functor [] =
  let map = lmap;
;

map (3*) (Just 6);
map (3+) [1, 2];
```

This should result in:

```javascript
// 'map' is now seen as an overloaded function

var __mapMaybe__ = function (f) {
  return function (m) {
    if (m[0] === "Nothing") {
      return Nothing;
    } else if (m[0] === "Just") {
      var x = m[1];
      return Just(f(x));
    }
  };
};

var __mapList__ = lmap;

__mapMaybe__(mult(3))(Just(6));
__mapList__(add(3))(_cons(1)(_cons(2)(_nil)))
```

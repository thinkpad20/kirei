Javascript functions take multiple arguments. But the lambda calculus, upon which Kirei is built, only take one argument. However, the lambda calculus allows tuples to pass as single values, which means that a function that takes *k* arguments can be modeled as one that takes a single tuple of *k* values.

```javascript
var foo = function (a, b, c) {
  return a + b + c;
};
```

Can be expressed in Kirei as

```kirei
let foo (a, b, c) = a + b + c;
```

This requires a little bit of thought before we can be sure we can proceed. For example, in Kirei, we'll support pattern matching, which means that we need to be able to deal with a tuple as a pattern. E.g. the following:

```kirei
let foo (1, b, c) = b + c
|   foo (a, 6, c) = a * c
|   foo (a, b, c) = a + b + c;
```

Needs to become something like the following javascript:

```javascript
var foo = function(a, b, c) {
  if (a === 1) {
    return b + c;
  }
  else if (b === 6) {
    return a * c;
  }
  else {
    return a + b + c;
  }
};
```

That function was a bit of a softball; for example, what if we rename the arguments? The result should be the same:

```kirei
let foo (1, x, y) = x + y
|   foo (q, 6, p) = q * p
|   foo (a, b, c) = a + b + c;
```

This suggests a naming scheme independent of the names of the variables as given:

```javascript
var foo = function(_arg0, _arg1, _arg2) {
  if (_arg0 === 1) {
    x = _arg1; y = _arg2;
    return x + y;
  }
  else if (_arg2 === 6) {
    q = _arg0; p = _arg2;
    return q * p;
  }
  else {
    a = _arg0; b = _arg1; c = _arg2;
    return a + b + c;
  }
};
```

Then the next thing we need to tackle is passing these arguments flat when compiling into JavaScript. Basically, when calling a function using a tuple, we want to call it with multiple arguments, but when returning a tuple, it's a list. This might be simple, or tricky.

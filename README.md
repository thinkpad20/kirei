# Kirei
## A Mostly-Pure Functional Language

### Motivation

Haskell is an amazing language: with incredible expressive power coupled with great performance and excellent guarantees of safety, it's truly a great achievement. However, Haskell can be a very difficult language to use for beginners, and many things which are straightforward to do in imperative langauges, particularly in dealing with IO and mutable state, are entirely different in Haskell, demanding a great deal of effort to gain facility.

In addition, its use of Monads for these concepts makes it difficult to translate Haskell to other high-level languages in a straightforward manner. For example, take some simple code like

```haskell
getTyped = do
  putStrLn "Write something:"
  s <- getLine
  putStrLn $ "You typed: " ++ s

main = getTyped

```

There's no straightforward translation of this into an imperative language. You might think it could be something like this:

```python
def getTyped():
    s = raw_input("Write something:\n")
    print "You typed: " + s

getTyped()
```

But while its syntax and outward behavior is similar, in truth the two are quite different, because under the surface the Haskell code is a single expression using a sophisticated system of monads, operators overloaded via type classes, and lambda functions. On the other hand, the Python is simply a series of instructions, which happen to be doing IO.

Kirei moves to take many of the best parts from Haskell, such as its static typing, type classes, operators-as-functions, pattern matching, and more, but use a different approach to maintaining functional purity and IO. Kirei handles these things through *tokens*, a system by which an argument is passed which does nothing on its own but

1. to signify that this function is authorized to perform the action requested, which helps us maintain funtional purity where desired, and
2. to distinguish between functions which perform some actions which have effects, and the actual running of those functions, which lets us deal with impure functions gracefully.

In Kirei, the code above might look something like this:

```haskell
let getTyped io =
  println "Write something:" io,
  let s = getLine io;
  println ("You typed: " ++ s) io;

getTyped $IO;
```

And its translation to an imperative language (we're currently using JavaScript) is straightforward (`println` and `getLine` here are of course not standard JS functions, but they wrap standard functions.):

```javascript
var getTyped = function (io) {
  println("Write something:")(io);
  var s = getLine(io);
  return println("You typed: " + s)(io);
};

getTyped($IO);
```

Note that `println("Write something:")(io)` is written in a curried manner (meaning that `println("foo")` returns a function, into which the next argument `io` is passed). If you haven't encountered currying before, it's basically a way to supply *some* of the arguments to a function, and produce a new function which takes the remaining arguments. So if `add a b = a + b`, then `add 5` is a function which takes one number and returns that number plus 5. Google for more details :). Also note that we also support standard multivariate functions as would be found in JavaScript, through tuples. We'll explain this more later.

### The Token System

Kirei is strictly evaluated by default, but attempts to meld that with a good degree of referential transparency. With its token system, its able to enforce a strong degree of purity in a way that's simple to understand. By extension, a key idea is that once provided with all of its arguments, a function should be considered to be completely determined, and its calculation can be performed. This works for both pure functions, and those with effects, via the token system.

Consider a basic factorial function. All we need to know to compute a factorial is its argument. This means that `fact` is a function which cannot yet be computed, but `fact 10` has all that it needs to know and can (and will, strictly) be computed.

On the other hand, what about a "void function", like `print`? Well, theoretically, `print "Hello, world!"` gives us all of the information we need to compute it (we just need to know what it is we want to print). What would this mean if we had a data structure full of print functions?

```haskell
printFuncs = [print "hello", print "world!"]
```

Well what we want is, for example, to be able to iterate through this structure, printing everything:

```
[f() for f in printFuncs]
```

But not only is this syntactically invalid, it doesn't type match! The type of `print` is presumably `print : String -> ()`, so in building `printFuncs`, having all of the arguments of the various `print` functions, we would compute their values -- printing to the screen and returning `()`. So that means `printFuncs` is not actually a list of functions, but just a list of `()`, which isn't very useful.

Kirei addresses this through its token system. We give `print` the following treatment:

```haskell
sig print : String -> IO -> ();
```

This is a signature in Kirei, meaning that `print` takes an `IO` token and a string, and returns a `()`. Now what we can do is this:

```haskell
sig printFuncs : [IO -> ()];
let printFuncs = [print "hello", print "world!"];
```

Now, we can see that `printFuncs` contains functions -- things waiting to be computed -- not values. So if we want to print all of these guys, we need to pass in an IO token! How can we do that?

```haskell
[p $IO | p <- printFuncs];
```

The `$IO` thingamajig is the top-level IO token: accessible only by module-scope functions (a.k.a. those declared outside of the scope of any subfunction), it's a global constant which allows module-scope functions to pass tokens to their subfunctions. More on this later. Anyway, this list comprehension means "take each element `p` from the list `printFuncs` and put the result of calling `p` with the argument `$IO` into a new list". We could also write this as

```haskell
map ($IO !) printFuncs;
```

With the definition:

```haskell
sig (!) : a -> (a -> b) -> b;
let a ! f = f a;
```

So essentially `!` means "feed what's on the left into the function on the right". Of course we could have also written `[$IO ! p | p <- printFuncs]`.

So we can see that this token system allows us to distinguish the definition of functions with effects from the running of those functions, which would produce effects. The same philosophy extends into mutable objects, which we'll treat later.

What the token system also accomplishes is a high degree of functional purity guarantees. IO is expressed in the type system, though not with monads as in Haskell. This means that a function which has not been passed an IO token as an argument cannot produce IO:

```
sig fireMissiles : Int -> IO -> ();
let fireMissiles = # does some horrible thing

# this lets us string things together
sig (,) : a -> b -> b;
let a, b = b;

sig bar : Int -> Int;
let bar a = fireMissiles a $IO, a + 1; # ILLEGAL

sig foo : Int -> Int;
let foo a = fireMissiles a, a + 1; # OK, fireMissiles has no effect
```

We have a rule that `IO` tokens cannot be returned inside of closures. This makes `bar` illegal, and for the exact reason we want: otherwise we'd have no way of knowing if `bar` did some side-effecty thing like firing missiles before it returned some innocuous number. Basically, tokens are only allowed in completely specified expressions. `let contents = getContents "foo.txt" $IO` is ok. But `let unsafeGetContents = \s => getContents s $IO` is not OK, because a token is being "wrapped up" in a container which hides its existence. A good algorithm to properly detect and make illegal this sort of usage is one of the things I need to work on, but it should be doable.

Now on the other hand, `foo` is fine. `fireMissiles a` is only a lambda expression, which would mean it would produce no side-effect. If it didn't perform IO and `a` were all it needed to run, then its value would be computed but not used; this is because we're a strict language. The unnecessary computation might be costly (indeed, it may not terminate), but it won't produce side-effects. Of course, compile-time optimizations will likely be able to remove many or all unused function calls. A benefit of functional purity is that if a function can be proven to be pure, and a call to it is made but not used, we can guarantee that the removal of the call will not affect the programs' correctness.

### Quick syntax overview:

Kirei's syntax is very simple and mostly defined by the lambda calculus.

We start with simple constants or conditional expressions:

```
1;
3 + 4;
if True then 5 else 87;
```

A definition, which is an immutable association between an identifier and an expression, can be written with `let`:

```
let a = 1;
let b = a + 3;
let c = if foobar == 0 then a else abs (max a b);
```

Note that expressions cannot contain `let` statements internally:

```
let foo = let bar = 3; bar + 4; # ERROR
```

The `let` and `;` must match like opening and closing parentheses. This association lets us ignore whitespace entirely.

Functions are another kind of expression, namely lambda expressions, and can be defined with the following syntax:

* `\`
* one or more arguments (identifiers)
* `->`
* the expression to be returned

So, for example:

```haskell
let decr = \a -> a - 1;
let fact1 = \n -> if n < 2 then 1 else n * (fact1 (decr n));
let fact2 = \n ->
  let factR = \n acc -> if n < 2 then acc else factR (n - 1) (acc * n);
  factR n;
```

You can also write functions without a lambda expression using a Haskell/ML-style syntax:

```haskell
let fib n = if n < 1 then 0 else if n < 2 then 1 else fib (n - 1) + fib (n - 2);
# the following is equivalent
let fib' = \n -> if n < 1 then 0 else if n < 2 then 1 else fib (n - 1) + fib (n - 2);
```

Single-line comments start with `#`; there are no block comments yet.

```
#here's a comment
here is some code;
```

Pattern matching works via Haskell-style case expressions:

```haskell
let fib n = case n of
  0 -> 0
| 1 -> 1
| n -> (fib (n-1)) + (fib (n-2));
```

You can declare algebraic data types with a Haskell-esque syntax:

```
datatype List a =
  Empty
| Cons a (List a);

let reverse list =
  let loop list2 accumulator = case list2 of
    Empty -> accumulator
  | Cons a as -> loop as (Cons a accumulator);
  loop list Empty;

let foo = [1..6];
let bar = [5,4,3,2,1];
assert foo == bar;
```

We have two forms of string interpolation. Using `#{ }` inside of a string will call `show` on whatever is inside the curly braces, while using `#[ ]` will put the result in verbatim (it must be a string).

```haskell
let name = "Allen";
let age = 28;
let intro1 = "Hi, my name is #[name] and I'm #{age} years old.";
let intro2 = "Hi, my name is " ++ name ++ " and I'm " ++ show age ++ " years old.";
assert intro1 == intro2;
```

And that's about it. Of course, future syntax will be introduced for type signatures, more syntactic sugar, and a bit more. But that's close to everything.

### Current status and usage

Right now we really don't do very much. We have a tiny "standard library" with a few curried arithmetical and logical functions (+, -, <, or, etc). We're compiling to JavaScript, which is nothing new but offers many advantages like relative ease of code generation and many use cases (pure languages compiled to JavaScript have been done, but tend to produce code which is difficult to read, often for the reasons above). There is still a vast amount to do, but at the least, we can write a factorial function which runs (with some supporting standard functions defined, that is). And of course, other functions can be written as well :).

The Kirei compiler is written in Haskell and requires a Haskell platform (google that if you don't have it -- but I'm guessing you do). To set up Kirei, download the source and compile it as follows:

```
> git clone https://github.com/thinkpad20/kirei
> cd kirei
> ghc -o bin/kirei src/Kirei.hs src/Parser.hs src/CompileJS.hs src/JavaScript/AST.hs
```

Then you can try writing a simple Kirei test script. Make sure you put it in the same folder as `std.js`, because otherwise it won't work (for now). There's already one of these in `first.kr`:

```haskell
let fact n = if n < 2 then 1 else n * (fact (n - 1));

let fib n =
  let f m acc prev =
    if m < 1 then prev
    else if m < 2 then acc
    else f (m-1) (acc + prev) (acc);
  f n 1 0;

std.writeln ("Factorial of 15: " + (fact 15)) $IO;
std.writeln ("Fibonacci of 100: " + (fib 100)) $IO;
```

You can compile and run this with:

```
> bin/kirei lib/first.kr
Wrote output to lib/first.js
> node lib/first.js
Fibonacci of 100: 354224848179262000000
Factorial of 15: 1307674368000
```

You can see the JavaScript generated:

```javascript
var std = require("./std");
var $IO = 0;

var fact = function (n) {
  if (std.lt(n)(2.0)) {
    return 1.0;
  }
  else {
    return std.mult(n)(fact(std.sub(n)(1.0)));
  }
};
var fib = function (n) {
  var f = function (m) {
    return function (acc) {
      return function (prev) {
        if (std.lt(m)(1.0)) {
          return prev;
        }
        else {
          if (std.lt(m)(2.0)) {
            return acc;
          }
          else {
            return f(std.sub(m)(1.0))(std.add(acc)(prev))(acc);
          }
        }
      };
    };
  };
  return f(n)(1.0)(0.0);
};
var f = std.writeln(std.add("Factorial of 15: ")(fact(15.0)))(std.writeln(std.add("Fibonacci of 100: ")(fib(100.0)))($IO));
```

If you want to tinker in GHCi, a good place to start is with the `toJs` function, which parses one or more Kirei statements and converts them into a JavaScript `Block`. This has an instance of `Show`, so it can be printed as valid JavaScript. However, to pretty-print the output, use `prettyPrintJS` (or `renderJS` if you want the escape sequences).

```
> cd src
> ghci
[... omitted ...]
Prelude> :load CompileJS.hs
[... omitted ...]
*CompileJS> let src = "let fact = \\n => if n < 2 then 1 else n * (fact (n - 1));"
*CompileJS> toJs src
var fact = function (n) {if (std.lt(n)(2.0)) {return 1.0;} else {return std.mult(n)(fact(std.sub(n)(1.0)));}};
*CompileJS> renderJS src
"\nvar fact = function (n) {\n  if (std.lt(n)(2.0)) {\n    return 1.0;\n  }\n  else {\n    return std.mult(n)(fact(std.sub(n)(1.0)));\n  }\n};"
*CompileJS> prettyPrintJS src

var fact = function (n) {
  if (std.lt(n)(2.0)) {
    return 1.0;
  }
  else {
    return std.mult(n)(fact(std.sub(n)(1.0)));
  }
};
*CompileJS>
```

Yay! Note that when writing a `\` in GHCi you need to write it with two backslashes so that it doesn't interpret it as an escape sequence. Of course, in a source file, this will not be done.

## Things left to do

A lot!

* All symbolic operators are currently right-associative, which makes some things require parenthesis where they shouldn't. We currently lack the ability to specify these.
* Standard library is non-existent (ALMOST non-existent. Some basic arithmetic, logical and IO functions are defined)
* No arrays or any builtin data structures (linked lists and maps are especially important)
  - We do have tuples now, woo hoo!
  - And now we have ADTs too, which means Lists! Although they need a little more work. We have a basic non-balanced BST now.
* No type system yet (we plan to implement Hindley-Milner, or a modified version thereof)
  - We have some a basic implementation of HM up and running, although it do much more than the pure lambda calculus yet. Working on it; it's tougher than it looks.
  - The postulated token system should be really just predicated on the type system, with a few small extensions
  - Type classes will also depend on this
* No TCO or any other optimizations
* A REPL would be nice (but would probably require a full implementation separate from simple javascript compilation)
* modules, namespaces, imports, etc...

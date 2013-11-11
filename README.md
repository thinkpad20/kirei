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

```
let getTyped io =
  println "Write something:" io,
  let s = getLine io;
  println ("You typed: " ++ s) io;

getTyped $IO;
```

And its translation to an imperative language (we're currently using JavaScript) is straightforward (assume all functions have been defined somewhere):

```javascript
var getTyped = function (io) {
  println("Write something:", io);
  var s = getLine(io);
  return println("You typed: " + s, io);
};

getTyped($IO);
```

(`println` and `getLine` here are of course not standard JS functions, but they wrap standard functions.)

### The Token System

Kirei is strictly evaluated by default, but attempts to meld that with a good degree of referential transparency. With its token system, its able to enforce a strong degree of purity in a way that's simple to understand. By extension, a key idea is that once provided with all of its arguments, a function should be considered to be completely determined, and its calculation can be performed. This works for both pure functions, and those with effects, via the token system.

Consider a basic factorial function. All we need to know to compute a factorial is its argument. This means that `fact` is a function which cannot yet be computed, but `fact 10` has all that it needs to know and can (and will, strictly) be computed.

On the other hand, what about a "void function", like `print`? Well, theoretically, `print "Hello, world!"` gives us all of the information we need to compute it (we just need to know what it is we want to print). What would this mean if we had a data structure full of print functions?

```
printFuncs = [print "hello", print "world!"]
```

Well what we want is, for example, to be able to iterate through this structure, printing everything:

```
[f() for f in printFuncs]
```

But not only is this syntactically invalid, it doesn't type match! The type of `print` is presumably `print : String -> ()`, so in building `printFuncs`, having all of the arguments of the various `print` functions, we would compute their values -- printing to the screen and returning `()`. So that means `printFuncs` is not actually a list of functions, but just a list of `()`, which isn't very useful.

Kirei addresses this through its token system. We give `print` the following treatment:

```
sig print : String -> IO -> ();
```

This is a signature in Kirei, meaning that `print` takes an `IO` token and a string, and returns a `()`. Now what we can do is this:

```
sig printFuncs : [IO -> ()];
let printFuncs = [print "hello", print "world!"];
```

Now, we can see that `printFuncs` contains functions -- things waiting to be computed -- not values. So if we want to print all of these guys, we need to pass in an IO token! How can we do that?

```
[p $IO | p <- printFuncs];
```

The `$IO` thingamajig is the top-level IO token: accessible only by module-scope functions (a.k.a. those declared outside of the scope of any subfunction), it's a global constant which allows module-scope functions to pass tokens to their subfunctions. More on this later. Anyway, this list comprehension means "take each element `p` from the list `printFuncs` and put the result of calling `p` with the argument `$IO` into a new list". We could also write this as

```
map ($IO !) printFuncs;
```

With the definition:

```
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

Now on the other hand, `foo` is fine. `fireMissiles a` is only a lambda expression, which would mean it would produce no side-effect. If it didn't perform IO and `a` were all it needed to run, then its value would be computed but not used; this is because we're a strict language. The unnecessary computation might be costly (indeed, it may not terminate), but it won't produce side-effects. Of course, compile-time optimizations will likely be able to remove many or all unused function calls, bringing the performance in line with a lazily evaluated language.

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

Expressions can contain `let` statements internally:

```
let foo = let bar = 3; bar + 4;
```

The `let` and `;` must match like opening and closing parentheses. This association lets us ignore whitespace entirely.

Functions are another kind of expression, namely lambda expressions, and can be defined with the following syntax:

* `\` 
* one or more arguments (identifiers)
* `=>`
* the expression to be returned

So, for example:

```
let decr = \a => a - 1;
let fact1 = \n => if n < 2 then 1 else n * (fact1 (decr n));
let fact2 = \n =>
  let factR = \n acc => if n < 2 then acc else factR (n - 1) (acc * n);
  factR n;
```

And that's about it. Of course, future syntax will be introduced for comments, pattern matching, type signatures, type and class declarations, data structure literals, etc.

### Current status and usage

Right now we really don't do very much. We're compiling to JavaScript, which is nothing new but offers many advantages like relative ease of code generation and many use cases (pure languages compiled to JavaScript have been done, but tend to produce code which is difficult to read, often for the reasons above). There is still a vast amount to do, but at the least, we can write a factorial function which runs (with some supporting standard functions defined, that is).

If you have a Haskell platform, you should be good to go. You can try it out like so:

```
> git clone <this repo's url>
> cd kirei/src
> ghci
[... omitted ...]
Prelude> :load CompileJS.hs
[... omitted ...]
*CompileJS> toJs "let fact = \\n => if n < 2 then 1 else n * (fact (n - 1));"
[... omitted ...]
var fact = function (n) {if (lt(n)(2.0)) {return 1.0;} else {return mult(n)(fact(sub(n)(1.0)));}};
*CompileJS>
```

Yay! Note that when writing a `\` in GHCi you need to write it with two backslashes so that it doesn't interpret it as an escape sequence. Of course, in a source file, this will not be done.

## Things left to do

A lot!

* right now all functions are single-argument (we'll later change this for performance and ease of readability)
* ~~we don't yet support infix symbols (everything is in a lisp-like prefix notation)~~ done!
* no operator precedences, user-defined or otherwise. Use parentheses to disambiguate.
* functions must be declared as lambdas (you can't write `let foo a = a + 1;`, you need to write `let foo = \a => a + 1;`) 
* ~~We can't produce javascript files, just some basic code~~ done! But not very robust
* haven't yet figured out how we're going to support JavaScript objects and `.` notation in a functional way
* Standard library is non-existent (ALMOST non-existent. Basic arithmetic and logical functions are defined)
* No pretty printing of the generated javascript code
* No arrays or any builtin data structures
* No type system yet
* No pattern matching yet
* No TCO or any other optimizations
* Token system hasn't been implemented yet
* A REPL would be nice (but would probably require a full implementation separate from simple javascript compilation)
* modules, namespaces, etc...

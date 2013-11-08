# Kirei
## A Mostly-Pure Functional Language

### Motivation

Haskell is an amazing language: with incredible expressive power coupled with great performance and excellent guarantees of safety, it's truly a great achievement. However, Haskell can be a very difficult language to use for beginners, and many things which are straightforward to do in imperative langauges, particularly in dealing with IO and mutable state, are entirely different in Haskell, demanding a great deal of effort to gain facility.

In addition, its use of Monads for these concepts makes it difficult to translate Haskell to other high-level languages in a straightforward manner. For example, take some simple code like

```haskell
getTyped :: IO ()
getTyped = do
  putStr "Write something: "
  s <- getLine
  putStrLn $ "You typed: " ++ s

main = getTyped

```

There's no good translation of this into an imperative language. You might think it could be something like this:

```python
def getTyped():
    s = raw_input("Write something: ")
    print "You typed: " + s

if __name__ == "__main__": getTyped()
```

But in truth the two are quite different, because under the surface the Haskell code is a single expression using a sophisticated system of monads and lambda functions, while the Python is simply a series of instructions, which happen to be doing IO.

Kirei moves to take many of the best parts from Haskell, such as its static typing, type classes, operators-as-functions, pattern matching, and more, but use a different approach to maintaining functional purity and IO. Kirei handles these things through *tokens*, a system by which an argument is passed which does nothing on its own but (a) to signify that this function is authorized to perform the action requested, and (b) to distinguish between functions which perform some actions which have effects, and the actual running of those functions.

In Kirei, the code above might look something like this:

```
sig getTyped :: IO -> ();
let getTyped io =
  let s = getLine io;
  println ("You typed: " ++ s) io;

getTyped $IO;
```

And its translation to an imperative language (we're currently using JavaScript) is straightforward:

```javascript
var getLine = function () {
  var s = getLine();
  return println("You typed: " + s);
};
```

### The Token System

Kirei is strictly evaluated by default, but attempts to meld that with a good degree of referential transparency. With its token system, its able to enforce a strong degree of purity in a way that's simple to understand. By extension, a key idea is that once provided with all of its arguments, a function should be considered to be completely determined, and its calculation can be performed. This works for both pure functions, and those with effects, via the token system.

Consider factorial. All we need to know to compute a factorial is its argument. This means that `fact` is a function which cannot yet be computed, but `fact 10` has all that it needs to know and can (and will) be computed.

On the other hand, what about a "void function", like `print`? Well, theoretically, `print "Hello, world!"` gives us all of the information we need to compute it. What would this mean if we had a data structure full of print functions?

```
printFuncs = [print "hello", print "world!"]
```

Well what we want is, for example, to be able to iterate through this structure, printing everything:

```
[f() for f in printFuncs]
```

But not only is this syntactically invalid, it doesn't type match! The type of `print` is presumably `print : String -> ()`, so in because
in building `printFuncs`, having all of the arguments of the various `print` functions, we would compute their values -- printing to the screen and returning `()`. So that means `printFuncs` is not actually a list of functions, bue just a list of `()`, which isn't very useful.

Kirei addresses this through its token system. We give `print` the following treatment:

```
sig print : String -> IO -> ();
```

This is a signature in Kirei, meaning that `print` takes an `IO` token and a string, and returns a `()`. Now what we can do is this:

```
sig printFuncs : [IO -> ()];
let printFuncs = [print "hello", print "world!"];
```

Now, we can see that `printFuncs` contains functions, not values. So if we want to print all of these guys, we need to pass in an IO token! How can we do that?

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

So essentially `!` means "feed the what's on the left into the function on the right". Of course we could have also written `[$IO ! p | p <- printFuncs]`.

So we can see that this token system allows us to distinguish the definition of functions with effects from the running of those functions, which would produce effects. The same philosophy extends into mutable objects.

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
let foo a = fireMissiles a, a + 1; #OK, fireMissiles has no effect
```

We have a rule that `IO` tokens cannot be returned inside of closures. This makes `bar` illegal, and for the exact reason we want: otherwise we'd have no way of knowing if `bar` did some side-effecty thing before it returned some innocuous `()`. Basically, tokens are only allowed in completely specified expressions. `let contents = getContents "foo.txt" $IO` is ok. But `let unsafeGetContents = \s => getContents s $IO` is not OK, because a token is being "wrapped up" in a container which hides its existence. A good algorithm to properly detect and make illegal this sort of usage is one of the things I need to work on, but it should be doable.

Now on the other hand, `foo` is fine. `fireMissiles a` is only a lambda expression, which would mean it would produce no side-effect. If it didn't perform IO and `a` were all it needed to run, then its value would be computed but not used; this is because we're a strict language.

### Current status and usage

Right now we really don't do very much. We're compiling to JavaScript, which is nothing new but offers many advantages like relative ease of code generation and many use cases (pure languages compiled to JavaScript have been done, but tend to produce code which is difficult to read). Also, right now all functions are single-argument (we'll later change this for performance and ease of readability), we don't yet support infix symbols (everything is in a lisp-like prefix notation), and functions must be declared as lambdas (you can't write `let foo a = a + 1;`, you need to write `let foo = \a => + a 1;`).

If you have a Haskell platform, you should be good to go. You can try it out like so:

```
> git clone <this repo's url>
> cd kirei/src
> ghci
[... omitted ...]
Prelude> :load CompileJS.hs
[... omitted ...]
*CompileJS> toJs "let fact = \\n => if < n 2 then 1 else * n (fact (- n 1));"
[... omitted ...]
var fact = function (n) {if (lt(n)(2.0)) {return 1.0;} else {return mult(n)(fact(sub(n)(1.0)));}};
*CompileJS>
```

Yay!

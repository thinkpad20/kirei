var prnt = function(a) {
  console.log(a);
  return function(b) {
    prnt(b);
  };
};

prnt("hello")("world!");

var foo = function(x) {
  return function(y) {
    return function(z) {
      return;
    };
  };
};

var p = console.log;

foo(p("hey"))(p("ho"))(p("let's go!"));


// great! so that means that things will be
// properly evaluated, from left to right.

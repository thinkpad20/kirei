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
var std = require("./std");
var $IO = std.$IO;
var Cons = std.Cons;
var Empty = std.Empty;
var add = std.add;
var sub = std.sub;
var mult = std.mult;
var div = std.div;
var lt = std.lt;
var gt = std.gt;
var leq = std.leq;
var geq = std.geq;
var eq = std.eq;
var neq = std.neq;
var and = std.and;
var or = std.or;
var neg = std.neg;
var mkList = std.mkList;
var mkListRange = std.mkListRange;
var defaultShow = std.defaultShow;
var printLn = std.printLn;
var print = std.print;

var increment = function (x) {
  return add(x)(1);
};

console.log(increment(5));var fact = function (n) {
  var f = function (n) {
    return function (acc) {
      if (lt(n)(2)) {
        return acc;
      } else {
        return f(sub(n)(1))(mult(acc)(n));
      }
    };
  };

  return f(n)(1);
};

console.log(fact(15));var fib = function (n) {
  var f = function (n) {
    return function (prev) {
      return function (prev2) {
        if (lt(n)(2)) {
          return prev;
        } else {
          return f(sub(n)(1))(add(n)(prev))(prev);
        }
      };
    };
  };

  return f(n)(1)(0);
};

console.log(fib(20));

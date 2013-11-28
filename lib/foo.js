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

var fib = function (n) {
  if (lt(n)(1)) {
    return 0;
  } else if (lt(n)(2)) {
    return 1;
  } else {
    return add(fib(sub(n)(1)))(fib(sub(n)(2)));
  }
};

var lttilde = function (f) {
  return function (g) {
    return function (x) {
      return f(g(x));
    };
  };
};

var fibPrint = lttilde(console.log)(fib);
fibPrint(10);


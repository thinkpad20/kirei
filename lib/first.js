var std = require("./std");

var fact = function (n) {if (std.lt(n)(2.0)) {return 1.0;} else {return std.mult(n)(fact(std.sub(n)(1.0)));}};console.log(fact(10.0));
var lt = function (a) { return function(b) { return a < b; }; };
var gt = function (a) { return function(b) { return a > b; }; };
var leq = function (a) { return function(b) { return a <= b; }; };
var geq = function (a) { return function(b) { return a >= b; }; };
var eq = function (a) { return function(b) { return a == b; }; };
var neq = function (a) { return function(b) { return a != b; }; };
var mult = function (a) { return function(b) { return a * b; }; };
var sub = function (a) { return function(b) { return a - b; }; };
var add = function (a) { return function(b) { return a + b; }; };
var div = function (a) { return function(b) { return a / b; }; };

var fact = function (n) {if (lt(n)(2.0)) {return 1.0;} else {return mult(n)(fact(sub(n)(1.0)));}};

console.log(fact(10));

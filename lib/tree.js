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

var Leaf = ["Leaf"];
var Branch = function (a0) {
  return function (treea1) {
    return function (treea2) {
      return ["Branch", a0, treea1, treea2];
    };
  };
};

var Yes = ["Yes"];
var No = ["No"];
var empty = Leaf;
var singleton = function (a) {
  return Branch(a)(Leaf)(Leaf);
};

var lookup = function (a) {
  return function (tree) {
    if (tree[0] === "Leaf") {
      return No;
    } else if (tree[0] === "Branch") {
      var b = tree[1];
      var left = tree[2];
      var right = tree[3];
      if (eq(a)(b)) {
        return Yes;
      } else if (lt(a)(b)) {
        return lookup(a)(left);
      } else {
        return lookup(a)(right);
      }
    } else {
      throw new Error("Pattern match failed");
    }
  };
};

var insert = function (a) {
  return function (tree) {
    if (tree[0] === "Leaf") {
      return Branch(a)(Leaf)(Leaf);
    } else if (tree[0] === "Branch") {
      var b = tree[1];
      var left = tree[2];
      var right = tree[3];
      if (eq(a)(b)) {
        return tree;
      } else if (lt(a)(b)) {
        return Branch(b)(insert(a)(left))(right);
      } else {
        return Branch(b)(left)(insert(a)(right));
      }
    } else {
      throw new Error("Pattern match failed");
    }
  };
};

printLn(defaultShow(insert(1)(singleton(2))))($IO);

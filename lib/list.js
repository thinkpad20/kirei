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

var reverse = function (list) {
  var rev = function (l) {
    return function (acc) {
      if (l[0] === "Empty") {
        return acc;
      } else if (l[0] === "Cons") {
        var a = l[1];
        var as = l[2];
        return rev(as)(Cons(a)(acc));
      } else {
        throw new Error("Pattern match failed");
      }
    };
  };
  return rev(list)(Empty);
};
var plusplus = function (list1) {
  return function (list2) {
    if (list1[0] === "Empty") {
      return list2;
    } else if (list1[0] === "Cons") {
      var a = list1[1];
      var as = list1[2];
      return Cons(a)(plusplus(as)(list2));
    } else {
      throw new Error("Pattern match failed");
    }
  };
};
var joinBy = function (sep) {
  return function (listOfLists) {
    if (listOfLists[0] === "Empty") {
      return Empty;
    } else if (listOfLists[0] === "Cons") {
      var a = listOfLists[1];
      var as = listOfLists[2];
      return Cons(a)(Cons(sep)(joinBy(sep)(as)));
    } else {
      throw new Error("Pattern match failed");
    }
  };
};
var single = function (a) {
  return Cons(a)(Empty);
};
var map = function (f) {
  return function (list) {
    if (list[0] === "Empty") {
      return Empty;
    } else if (list[0] === "Cons") {
      var a = list[1];
      var lst = list[2];
      return Cons(f(a))(map(f)(lst));
    } else {
      throw new Error("Pattern match failed");
    }
  };
};
var render = function (list) {
  return plusplus(single("["))(plusplus(joinBy(",")(list))(single("]")));
};
std.printList(map(add(1))(mkList(1, 2, 3, 6, 14, 30)));

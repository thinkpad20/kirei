var std = require("./std");
var $IO = 0;
var Empty = ["Empty"];
var Cons = function (a, list) {
  return ["Cons", a, list];
};
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

// small library of built-in functions that Kirei can use

var lt = exports.lt = function (a) {
  return function(b) {
    return a < b;
  };
};

var gt = exports.gt = function (a) {
  return function(b) {
    return a > b;
  };
};

var leq = exports.leq = function (a) {
  return function(b) {
    return a <= b;
  };
};

var geq = exports.geq = function (a) {
  return function(b) {
    return a >= b;
  };
};

var eq = exports.eq = function (a) {
  return function(b) {
    return a === b;
  };
};

var neq = exports.neq = function (a) {
  return function(b) {
    return a !== b;
  };
};

var mult = exports.mult = function (a) {
  return function(b) {
    return a * b;
  };
};

var sub = exports.sub = function (a) {
  return function(b) {
    return a - b;
  };
};

var add = exports.add = function (a) {
  return function(b) {
    return a + b;
  };
};

var div = exports.div = function (a) {
  return function(b) {
    return a / b;
  };
};

var and = exports.and = function (a) {
  return function(b) {
    return a && b;
  };
};

var or = exports.or = function (a) {
  return function(b) {
    return a || b;
  };
};

var neg = exports.neg = function(a) {
  return -a;
};

var write = exports.write = function(s) {
  return function(io) {
    process.stdout.write(s);
  };
};

var writeln = exports.writeln = function(s) {
  return function(io) {
    process.stdout.write(s + "\n");
  };
};

var pow = exports.pow = function(a) {
  return function(b) {
    return Math.pow(a, b);
  };
};

var $IO = exports.$IO = 0;

var Empty = exports.Empty = ["Empty"];

var Cons = exports.Cons = function (a) {
  return function(list) {
    return ["Cons", a, list];
  };
};

exports.mkList = function(/*args...*/) {
  var res = Empty;
  for (i = arguments.length - 1; i >= 0; i--) {
    res = Cons(arguments[i])(res);
  }
  return res;
};

exports.mkListRange = function(start, stop, step) {
  var res = Empty;
  while (start !== stop) {
    res = Cons(start)(res);
    if (step !== undefined) {
      start = step(start);
    } else if (start < stop) {
      start++;
    } else {
      start--;
    }
  }
  return res;
};

exports.printList = function(list) {
  process.stdout.write("[");
  var first = true;
  while (list[0] !== "Empty") {
    if (!first) {
      process.stdout.write(", ");
    } else {
      first = false;
    }
    process.stdout.write(list[1].toString());
    list = list[2];
  }
  process.stdout.write("]\n");
};

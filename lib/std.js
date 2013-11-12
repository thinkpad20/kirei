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

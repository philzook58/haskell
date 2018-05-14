"use strict";
var Additive = function (add$prime, zero$prime) {
    this["add'"] = add$prime;
    this["zero'"] = zero$prime;
};
var Multiplicative = function (mul$prime, one$prime) {
    this["mul'"] = mul$prime;
    this["one'"] = one$prime;
};
var zero$prime = function (dict) {
    return dict["zero'"];
};
var one$prime = function (dict) {
    return dict["one'"];
};
var mul$prime = function (dict) {
    return dict["mul'"];
};
var add$prime = function (dict) {
    return dict["add'"];
};
module.exports = {
    "add'": add$prime,
    "mul'": mul$prime,
    "one'": one$prime,
    "zero'": zero$prime,
    Additive: Additive,
    Multiplicative: Multiplicative
};

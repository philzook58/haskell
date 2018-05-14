"use strict";
var Factored = (function () {
    function Factored(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    Factored.create = function (value0) {
        return function (value1) {
            return new Factored(value0, value1);
        };
    };
    return Factored;
})();
module.exports = {
    Factored: Factored
};

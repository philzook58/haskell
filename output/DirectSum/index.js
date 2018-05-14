"use strict";
var DSum = (function () {
    function DSum(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    DSum.create = function (value0) {
        return function (value1) {
            return new DSum(value0, value1);
        };
    };
    return DSum;
})();
module.exports = {
    DSum: DSum
};

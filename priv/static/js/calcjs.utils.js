//var ihtx = ihtx|| {};
var jq = $;
var calc = {
    DEFAULT_ROUNDING_MODE: BigDecimal.ROUND_FLOOR,
    DEFAULT_SCALE: 20,
    MODE: {
        NATIVE: 1,
        ACCURATE: 2
    },

    abs:function(x) {
        var res;
        if (!calc.is_empty(x)) {
            res = Math.abs(calc.remove_commas(x, calc.MODE.NATIVE));
        }
        return res;
    },
    abs_acc:function(x) {
        var res;
        if (!calc.is_empty(x)) {
            res = calc.remove_commas(x, calc.MODE.ACCURATE).abs().toString();
        }
        return res;
    },
    acos:function(x) {
        var res;
        if (!calc.is_empty(x)) {
            res = Math.acos(calc.remove_commas(x, calc.MODE.NATIVE));
        }
        return res;
    },
    add:function() {
        var res;
        var array = arguments;
        if (arguments.length == 1 && arguments[0] instanceof Array) {
            array = arguments[0];
        }
        for (var i=0; i<array.length; i++) {
            if (!calc.is_empty(array[i])) {
                if (res !== 0 && !res) {
                    res = calc.remove_commas(array[i], calc.MODE.NATIVE);
                } else {
                    res += calc.remove_commas(array[i], calc.MODE.NATIVE);
                }
            }
        }
        return res;
    },
    add_acc:function() {
        var res;
        var array = arguments;
        if (arguments.length == 1 && arguments[0] instanceof Array) {
            array = arguments[0];
        }
        for (var i=0; i<array.length; i++) {
            if (!calc.is_empty(array[i])) {
                if (res !== 0 && !res) {
                    res = calc.remove_commas(array[i], calc.MODE.ACCURATE);
                } else {
                    res = res.add(calc.remove_commas(array[i], calc.MODE.ACCURATE));
                }
            }
        }
        if (res) {
            res = res.toString();
        }
        return res;
    },
    asin:function(x) {
        var res;
        if (!calc.is_empty(x)) {
            res = Math.asin(calc.remove_commas(x, calc.MODE.NATIVE));
        }
        return res;
    },
    atan:function(x) {
        var res;
        if (!calc.is_empty(x)) {
            res = Math.atan(calc.remove_commas(x, calc.MODE.NATIVE));
        }
        return res;
    },
    atan2:function(y, x) {
        var res;
        if (!calc.is_empty(y) && !calc.is_empty(x)) {
            res = Math.atan2(calc.remove_commas(y, calc.MODE.NATIVE), calc.remove_commas(x, calc.MODE.NATIVE));
        }
        return res;
    },
    ceil:function(x) {
        var res;
        if (!calc.is_empty(x)) {
            res = Math.ceil(calc.remove_commas(x, calc.MODE.NATIVE));
        }
        return res;
    },
    commafy:function(x) {
        var res;
        if (!calc.is_empty(x)) {
            var val = x.toString().split('.');
            
            val[0] = val[0].replace(/\B(?=(\d{3})+(?!\d))/g, ",");
            
            res = val.join('.');
        }
        return res;
    },
    cos:function(x) {
        var res;
        if (!calc.is_empty(x)) {
            res = Math.cos(calc.remove_commas(x, calc.MODE.NATIVE));
        }
        return res;
    },
    counts:function(list, condition) {
        return calc.add(list.map(function(val) {
            var res = 0;
            if (!calc.is_empty(val) && !calc.is_empty(condition)) {
                var conditionResult = eval(condition.replace(new RegExp("var", 'g'), "calc.remove_commas(val, calc.MODE.NATIVE)"));
                if (conditionResult) {
                    res = 1;
                }
            }
            return res;
        }));
    },
    divide:function(x, y) {
        var res;
        if (!calc.is_empty(x) && !calc.is_empty(y) && y != 0) {
            res = calc.remove_commas(x, calc.MODE.ACCURATE).divide(calc.remove_commas(y, calc.MODE.ACCURATE), calc.DEFAULT_SCALE, calc.DEFAULT_ROUNDING_MODE).toString();
        }
        return res;
    },
    exp:function(x) {
        var res;
        if (!calc.is_empty(x)) {
            res = Math.exp(calc.remove_commas(x, calc.MODE.NATIVE));
        }
        return res;
    },
    floor:function(x) {
        var res;
        if (!calc.is_empty(x)) {
            res = Math.floor(calc.remove_commas(x, calc.MODE.NATIVE));
        }
        return res;
    },
    get_repeating_field_array:function(fieldName) {
        var array = [];
        var elemNr = 1;

        while ($("#" + fieldName + "_" + elemNr).length ) {
            var f = $("#" + fieldName + "_" + elemNr);
            var value = f.attr('type') == "checkbox" ? f.prop('checked') : f.val();
            array.push(value);

            elemNr++;
        }

        return array;
    },
    is_empty:function(x) {
        var res = false;
        if (x === false || x === 0 || x) {
            if (typeof x === "string") {
                res = (x.trim().length == 0);
            }
        } else {
            res = true;
        }
        return res;
    },
    limit:function(lower_bound, x, upper_bound) {
        var ret = x;
        if (!calc.is_empty(x) && !calc.is_empty(lower_bound) && !calc.is_empty(upper_bound)) {
            if (lower_bound != "*" && calc.remove_commas(x, calc.MODE.NATIVE) < lower_bound) {
                ret = lower_bound;
            } else if (upper_bound != "*" && calc.remove_commas(x, calc.MODE.NATIVE) > upper_bound) {
                ret = upper_bound;
            }
        }
        return ret;
    },
    limit_over:function(left_val, right_val, x, lower_bound) {
        var ret = x;
        if (!calc.is_empty(left_val) && !calc.is_empty(right_val) && calc.remove_commas(left_val, calc.MODE.NATIVE) < calc.remove_commas(right_val, calc.MODE.NATIVE)) {
            ret = lower_bound;
        }
        return ret;
    },
    log:function(x) {
        var res;
        if (!calc.is_empty(x)) {
            res = Math.log(calc.remove_commas(x, calc.MODE.NATIVE));
        }
        return res;
    },
    max:function() {
        var res;
        var array = arguments;
        if (arguments.length == 1 && arguments[0] instanceof Array) {
            array = arguments[0];
        }
        for (var i=0; i<array.length; i++) {
            if (!calc.is_empty(array[i])) {
                var temp = calc.remove_commas(array[i], calc.MODE.NATIVE);
                if (res !== 0 && !res) {
                    res = temp;
                } else if (temp > res) {
                    res = temp;
                }
            }
        }
        return res;
    },
    min:function() {
        var res;
        var array = arguments;
        if (arguments.length == 1 && arguments[0] instanceof Array) {
            array = arguments[0];
        }
        for (var i=0; i<array.length; i++) {
            if (!calc.is_empty(arguments[i])) {
                var temp = calc.remove_commas(array[i], calc.MODE.NATIVE);
                if (res !== 0 && !res) {
                    res = temp;
                } else if (temp < res) {
                    res = temp;
                }
            } else {
                res = undefined;
                break;
            }
        }
        return res;
    },
    mod:function(x, y) {
        var res;
        if (!calc.is_empty(x) && !calc.is_empty(y) && y != 0) {
            res = calc.remove_commas(x, calc.MODE.NATIVE)%calc.remove_commas(y, calc.MODE.NATIVE);
        }
        return res;
    },
    multiply:function() {
        var res;
        var array = arguments;
        if (arguments.length == 1 && arguments[0] instanceof Array) {
            array = arguments[0];
        }
        for (var i=0; i<array.length; i++) {
            if (!calc.is_empty(array[i])) {
                if (res !== 0 && !res) {
                    res = calc.remove_commas(array[i], calc.MODE.NATIVE);
                } else {
                    res *= calc.remove_commas(array[i], calc.MODE.NATIVE);
                }
            } else {
                return undefined;
            }
        }
        return res;
    },
    multiply_acc:function() {
        var res;
        var array = arguments;
        if (arguments.length == 1 && arguments[0] instanceof Array) {
            array = arguments[0];
        }
        for (var i=0; i<array.length; i++) {
            if (!calc.is_empty(array[i])) {
                if (res !== 0 && !res) {
                    res = calc.remove_commas(array[i], calc.MODE.ACCURATE);
                } else {
                    res = res.multiply(calc.remove_commas(array[i], calc.MODE.ACCURATE));
                }
            } else {
                return undefined;
            }
        }
        if (res) {
            res = res.toString();
        }
        return res;
    },
    pow:function(x, y) {
        var res;
        if (!calc.is_empty(x) && !calc.is_empty(y)) {
            res = Math.pow(calc.remove_commas(x, calc.MODE.NATIVE), calc.remove_commas(y, calc.MODE.NATIVE));
        }
        return res;
    },
    quotient:function(x, y) {
        var res;
        if (!calc.is_empty(x) && !calc.is_empty(y) && y != 0) {
            var _x = calc.remove_commas(x, calc.MODE.NATIVE);
            var _y = calc.remove_commas(y, calc.MODE.NATIVE);
            var x_int_val = parseInt(_x);
            var y_int_val = parseInt(_y);
            if ((x_int_val >= 0 && y_int_val >=0) || (x_int_val <= 0 && y_int_val <= 0)) {
                res = Math.floor(_x/_y);
            } else {
                res = Math.ceil(_x/_y);
            }
        }
        return res;
    },
    remove_commas:function(value, mode) {
        var res = value;
        if (!calc.is_empty(value)) {
            if (typeof value === "string") {
                res = value.replace(/\,/g,'');
            }
            res = calc.to_number(res, mode);
        }
        return res;
    },
    round:function(x, y) {
        var res;
        if (!calc.is_empty(x) && !calc.is_empty(y)) {
            res = Math.round(calc.remove_commas(x, calc.MODE.NATIVE) * Math.pow(10, y))/Math.pow(10, y);
        }
        return res;
    },
    round_down:function(x, y) {
        var res;
        if (!calc.is_empty(x) && !calc.is_empty(y)) {
            if (y == 0) {
                res = Math.floor(calc.remove_commas(x, calc.MODE.NATIVE));
            } else if (y > 0) {
                res = calc.remove_commas(x, calc.MODE.ACCURATE).setScale(y, BigDecimal.ROUND_FLOOR).toString();
            } else {
                res = Math.floor(+(calc.remove_commas(x, calc.MODE.NATIVE))/Math.pow(10, -y)) * Math.pow(10, -y);
            }
        }
        return res;
    },
    round_up:function(x, y) {
        var res;
        if (!calc.is_empty(x) && !calc.is_empty(y)) {
            if (y == 0) {
                res = Math.ceil(calc.remove_commas(x, calc.MODE.NATIVE));
            } else if (y > 0) {
                res = calc.remove_commas(x, calc.MODE.ACCURATE).setScale(y, BigDecimal.ROUND_CEILING).toString();
            } else {
                res = Math.floor(parseInt(calc.remove_commas(x, calc.MODE.NATIVE))/Math.pow(10, -y) + 1) * Math.pow(10, -y);
            }
        }
        return res;
    },
    sin:function(x) {
        var res;
        if (!calc.is_empty(x)) {
            res = Math.sin(calc.remove_commas(x, calc.MODE.NATIVE));
        }
        return res;
    },
    sqrt:function(x) {
        var res;
        if (!calc.is_empty(x)) {
            res = Math.sqrt(calc.remove_commas(x, calc.MODE.NATIVE));
        }
        return res;
    },
    string_to_native:function(x) {
        var res;
        if (x && typeof x === "string") {
            var decimalPointIndex = x.indexOf(".");
            if (decimalPointIndex >= 0) {
                res = parseFloat(x);
            } else {
                res = parseInt(x);
            }
        }
        return res;
    },
    subtract:function() {
        var res;
        var array = arguments;
        if (arguments.length == 1 && arguments[0] instanceof Array) {
            array = arguments[0];
        }
        for (var i=0; i<array.length; i++) {
            if (!calc.is_empty(array[i])) {
                if (res !== 0 && !res) {
                    res = calc.remove_commas(array[i], calc.MODE.NATIVE);
                    if (i > 0) {
                        res = res * (-1);
                    }
                } else {
                    res -= calc.remove_commas(array[i], calc.MODE.NATIVE);
                }
            }
        }
        return res;
    },
    subtract_acc:function() {
        var res;
        var array = arguments;
        if (arguments.length == 1 && arguments[0] instanceof Array) {
            array = arguments[0];
        }
        for (var i=0; i<array.length; i++) {
            if (!calc.is_empty(array[i])) {
                if (res !== 0 && !res) {
                    res = calc.remove_commas(array[i], calc.MODE.ACCURATE);
                    if (i > 0) {
                        res = res.negate();
                    }
                    
                } else {
                    res = res.subtract(calc.remove_commas(array[i], calc.MODE.ACCURATE));
                }
            }
        }
        if (res) {
            res = res.toString();
        }
        return res;
    },
    sum_if:function(condition, elements) {
        var res;
        if (condition) {
            res = calc.add(elements);
        }
        return res;
    },
    sum_if_acc:function(condition, elements) {
        var res;
        if (condition) {
            res = calc.add_acc(elements);
        }
        return res;
    },
    tan:function(x) {
        var res;
        if (!calc.is_empty(x)) {
            res = Math.tan(calc.remove_commas(x, calc.MODE.NATIVE));
        }
        return res;
    },
    to_number:function(x, mode) {
        var res;
        switch(mode) {
            case calc.MODE.NATIVE:
                if (typeof x === "number") {
                    res = x;
                } else {
                    res = calc.string_to_native(typeof x === "string" ? x : x.toString());
                }
                break;
            case calc.MODE.ACCURATE:
                res = new BigDecimal(typeof x === "string" ? x : x.toString());
                break;
            default:
                res = new BigDecimal(typeof x === "string" ? x : x.toString());
        }
        return res;
    }
};

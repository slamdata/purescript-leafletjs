var L = require("leaflet");

exports.any3d_ = function() {
    return function() {
        return L.Browser.any3d;
    };
};

exports.testProp_ = function(Nothing, Just, arr) {
    return function() {
        var res = L.DomUtil.testProp(arr);
        if (res === false) {
            return Nothing;
        } else {
            return Just(res);
        }
    };
};


exports.setStyle_ = function(key, val, el) {
    return function() {
        el.style[key] = val;
    };
};

exports.setPosition_ = function(el, point) {
    return function() {
        L.DomUtil.setPosition(el, {x: point[0], y: point[1]});
    };
};

exports.setTransform_ = function(el, point, scale) {
    return function() {
        L.DomUtil.setTransform(el, {x: point[0], y: point[1]}, scale);
    };
};

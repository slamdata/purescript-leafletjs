var L = require("leaflet");

exports.layers_ = function(baseLayers, overlays, options) {
    return function() {
        return L.control.layers(baseLayers, overlays, options);
    };
};

exports.addTo_ = function(map, control) {
    return function() {
        return control.addTo(map);
    };
};

exports.remove_ = function(control) {
    return function() {
        return control.remove();
    };
};

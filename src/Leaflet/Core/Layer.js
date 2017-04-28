var L = require("leaflet");


var preparations = function(converter) {
    return [
        { tag: "pane" },
        { tag: "attribution" },
        { tag: "interactive" },
        { tag: "iconUrl", fn: converter.printURI },
        { tag: "iconRetinaUrl", fn: converter.printURI },
        { tag: "iconSize", fn: converter.mkPoint },
        { tag: "iconAnchor", fn: converter.mkPoint },
        { tag: "popupAnchor", fn: converter.mkPoint },
        { tag: "shadowUrl", fn: converter.printURI },
        { tag: "shadowRetinaUrl", fn: converter.printURI },
        { tag: "shadowSize", fn: converter.mkPoint },
        { tag: "shadowAnchor", fn: converter.mkPoint },
        { tag: "className" },
        { tag: "offset", fn: converter.mkPoint },
        { tag: "maxWidth" },
        { tag: "minWidth" },
        { tag: "maxHeight" },
        { tag: "minHeight" },
        { tag: "autoPan" },
        { tag: "autoPanPaddingTopLeft" },
        { tag: "autoPanPadddingBottomRight" },
        { tag: "autoPanPadding" },
        { tag: "keepInView" },
        { tag: "closeButton" },
        { tag: "closeOnClick" },
        { tag: "autoClose" },
        { tag: "stroke" },
        { tag: "color", fn: converter.printColor },
        { tag: "weight" },
        { tag: "opacity" },
        { tag: "lineCap", fn: converter.printLineCap },
        { tag: "lineJoin", fn: converter.printLineJoin },
        { tag: "dashArray", fn: converter.printDashArray },
        { tag: "dashOffset", fn: converter.printPercentOrPixel },
        { tag: "fill" },
        { tag: "fillColor", fn: converter.printColor },
        { tag: "fillOpacity" },
        { tag: "fillRule", fn: converter.printFillRule },
        { tag: "renderer" },
        { tag: "smoothFactor" },
        { tag: "noClip" },
        { tag: "radius" }
    ];
};

var prepareConf = function(converter, conf) {
    var res = {},
        preps = preparations(converter),
        prep, tag, fn, i;

    for (var i = 0; i < preps.length; i++) {
        prep = preps[i];
        tag = prep.tag;
        fn = prep.fn;
        if (conf.hasOwnProperty(tag)) {
            res[tag] = fn ? fn(conf[tag]) : conf[tag];
        }
    }
    return res;
};

exports.layer_ = function() {
    return new L.Layer();
};

exports.tileLayer_ = function(str) {
    return function() {
        return L.tileLayer(str);
    };
};

exports.marker_ = function(latlng) {
    return function() {
        return L.marker(latlng);
    };
};

exports.icon_ = function(converter, conf) {
    return function() {
        return L.icon(prepareConf(converter, conf));
    };
};

exports.popup_ = function(converter, conf) {
    return function() {
        return L.popup(prepareConf(converter, conf));
    };
};

exports.setLatLng_ = function(latlng, popup) {
    return function() {
        return popup.setLatLng(latlng);
    };
};

exports.setContent_ = function(c, popup) {
    return function() {
        return popup.setContent(c);
    };
};


exports.openOn_ = function(map, popup) {
    return function() {
        return popup.openOn(map);
    };
};

exports.setIcon_ = function(icon, marker) {
    return function() {
        return marker.setIcon(icon);
    };
};

exports.bindPopup_ = function(c, marker) {
    return function() {
        return marker.bindPopup(c);
    };
};

exports.openPopup_ = function(isJust, latlng, layer) {
    return function() {
        var ll = isJust ? latlng : undefined;
        return layer.openPopup(ll);
    };
};



exports.circleMarker_ = function(latlng, converter, conf) {
    return function() {
        return L.circleMarker(latlng, prepareConf(converter, conf));
    };
};

exports.circle_ = function(latlng, converter, conf) {
    return function() {
        return L.circle(latlng, prepareConf(converter, conf));
    };
};


exports.polyline_ = function(latlngs, converter, conf) {
    return function() {
        return L.polyline(latlngs, prepareConf(converter, conf));
    };
};

exports.polygon_ = function(latlngs, converter, conf) {
    return function() {
        return L.polygon(latlngs, prepareConf(converter, conf));
    };
};

exports.rectangle_ = function(latlngs, converter, conf) {
    return function() {
        return L.rectangle(latlngs, prepareConf(converter, conf));
    };
};


exports.on_ = function(e, cb, l) {
    return function() {
        l.on(e, function() {
            cb(e)();
        });
    };
};

exports.addLayer_ = function(layer, leaflet) {
    return function() {
        layer.addTo(leaflet);
        return leaflet;
    };
};

exports.removeLayer_ = function(layer, leaflet) {
    return function() {
        leaflet.removeLayer(layer);
        return leaflet;
    };
};

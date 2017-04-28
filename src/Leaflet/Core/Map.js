var L = require("leaflet");

exports.map_ = function(el) {
    return function() {
        return L.map(el);
    };
};

exports.setView_ = function(latLng, leaflet) {
    return function() {
        return leaflet.setView(latLng);
    };
};

exports.setZoom_ = function(zoom, leaflet) {
    return function() {
        return leaflet.setZoom(zoom);
    };
};


exports.getSize_ = function(Tuple, leaflet) {
    return function() {
        var res = leaflet.getSize();
        return Tuple(res.x)(res.y);
    };
};



exports.zoomAnimation_ = function(leaflet) {
    return function() {
        return leaflet.options.zoomAnimation;
    };
};

exports.getPanes_ = function(leaflet) {
    return function() {
        return leaflet._panes;
    };
};

exports.containerPointToLayerPoint_ = function(Tuple, point, leaflet) {
    return function() {
        var res = leaflet.containerPointToLayerPoint(point);
        return Tuple(res.x)(res.y);
    };
};

exports.latLngToContainerPoint_ = function(Tuple, latLng, leaflet) {
    return function() {
        var res = leaflet.latLngToContainerPoint(latLng);
        return Tuple(res.x)(res.y);
    };
};

exports.getZoomScale_ = function(zoom, leaflet) {
    return function() {
        return leaflet.getZoomScale(zoom);
    };
};



exports.getMapPanePos_ = function(Tuple, leaflet) {
    return function() {
        var res = leaflet._getMapPanePos();
        return Tuple(res.x)(res.y);
    };
};

exports.getCenterOffset_ = function(Tuple, point, leaflet) {
    return function() {
        var res = leaflet._getCenterOffset({x: point[0], y: point[1]});
        return Tuple (res.x) (res.y);
    };
};



exports.getMaxZoom_ = function(leaflet) {
    return function() {
        return leaflet.getMaxZoom();
    };
};

exports.getZoom_ = function(leaflet) {
    return function() {
        return leaflet.getZoom();
    };
};

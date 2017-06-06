exports.eventCenter_ = function(Nothing, Just, Tuple, e) {
    return function() {
        if (e.center === undefined) {
            return Nothing;
        } else {
            return Just(Tuple(e.center.x)(e.center.y));
        }
    };
};

exports.eventZoom_ = function(Nothing, Just, e) {
    return function() {
        if (e.zoom === undefined) {
            return Nothing;
        } else {
            return Just(e.zoom);
        }
    };
};

exports.eventContainerPoint_ = function(Nothing, Just, Tuple, e) {
    return function() {
        if (e.containerPoint === undefined) {
            return Nothing;
        } else {
            return Just(Tuple(e.containerPoint.x)(e.containerPoint.y));
        }
    };
};

exports.eventLatLng_ = function(Nothing, Just, e) {
    return function() {
        if (e.latlng === undefined) {
            return Nothing;
        } else {
            return Just(e.latlng);
        }
    };
};

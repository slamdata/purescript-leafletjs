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

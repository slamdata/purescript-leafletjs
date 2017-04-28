exports.onload = function(ef) {
    return function() {
        window.onload = function() {
            return ef();
        };
    };
};



exports.debugTime = function(tag) {
    return function(act) {
        return function() {
            console.time(tag);
            var res = act();
            console.timeEnd(tag);
            return res;
        };
    };
};

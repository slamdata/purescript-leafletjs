exports.onload = function(ef) {
    return function() {
        window.onload = function() {
            return ef();
        };
    };
};

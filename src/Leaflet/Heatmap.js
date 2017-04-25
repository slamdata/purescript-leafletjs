exports.modifyImageData = function(imgData, modifier) {
//    return function(modifier) {
        imgData.data.set(modifier(imgData.data));
        return imgData;
//    };
};

exports.unsafePokeSTArray = function(arr, ix, val) {
    return function() {
        arr[ix] = val;
    };
};

exports.forEUC = function(low, hi, f) {
    return function() {
        for (var i = low; i < hi; i++) {
            f(i)();
        }
    };
};

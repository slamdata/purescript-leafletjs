exports.modifyImageData = function(imgData, modifier) {
        imgData.data.set(modifier(imgData.data));
        return imgData;
};

exports.unsafeSet = function(ix, val, arr) {
    arr[ix] = val;
    return arr;
};

exports.unsafeGet = function(ix, arr) {
    return arr[ix];
};

exports.unsafeFor = function(low, hi, arr, fn) {
    var res = arr;
    for (var i = low; i < hi; i++) {
        res = fn(i);
    }
    return res;
};

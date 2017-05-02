exports.onAddRemove_ = function(Nothing, Just, newRef, writeRef, onAdd, onRemove, layer, map) {
    return function() {
        var res = newRef(Nothing)();
        layer.onAdd = function(map) {
            var value = onAdd(this, map)();
            writeRef(res)(value)();
            return this;
        };
        layer.onRemove = function(map) {
            onRemove(this, map, res.value)();
            return this;
        };
        return res;
    };
};

exports.onAddRemove_ = function(Nothing, Just, onAdd, onRemove, layer, map) {
    return function() {
        var res = {value: Nothing};
        layer.onAdd = function(map) {
            res.value = onAdd(this, map)();
            return this;
        };
        layer.onRemove = function(map) {
            onRemove(this, map, res.value)();
            return this;
        };
        return res;
    };
};

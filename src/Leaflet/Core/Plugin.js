exports.onAddRemove_ = function(onAdd, onRemove, layer, map) {
    return function() {
        layer.onAdd = function(map) {
            onAdd(this, map)();
            return this;
        };
        layer.onRemove = function(map) {
            onRemove(this, map)();
            return this;
        };
    };
};

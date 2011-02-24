App.Views.Index = Backbone.View.extend({
    initialize: function() {
        this.images = this.options.images;
        this.render();
    },

    render: function() {
        if(this.images.length > 0) {
            var out = "<h3><a href='#new'>Create New</a></h3><ul>";
            _(this.images).each(function(item) {
                out += "<li><a href='#images/" + item.id + "'><img src='" + item.get('thumbnail') + "' /></a></li>";
            });
            out += "</ul>";
        } else {
            out = "<h3>No images! <a href='#new'>Create one</a></h3>";
        }
        $(this.el).html(out);
        $('#app').html(this.el);
    }
});

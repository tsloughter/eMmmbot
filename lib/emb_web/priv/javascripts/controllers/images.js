App.Controllers.Images = Backbone.Controller.extend({
    routes: {
        "images/:id":            "edit",
        "":                         "index",
        "new":                      "newDoc"
    },

    edit: function(id) {
        var doc = new Image({ id: id });
        doc.fetch({
            success: function(model, resp) {
                new App.Views.Edit({ model: doc });
            },
            error: function() {
                new Error({ message: 'Could not find that document.' });
                window.location.hash = '#';
            }
        });
    },

    index: function() {
        $.getJSON('/images', function(data) {
            if(data) {
                var images = _(data).map(function(i) { return new Image(i); });
                new App.Views.Index({ images: images });
            } else {
                new Error({ message: "Error loading images." });
            }
        });
    },

    newDoc: function() {
        new App.Views.Edit({ model: new Image() });
    }
});

App.Controllers.Images = Backbone.Controller.extend({
    routes: {
        "images/:id":               "edit",
        "tagcloud":                 "tagcloud",
        "":                         "index",
        "prev/:page":               "prev",
        "next/:page":               "next",
        "first":                    "first",
        "last":                     "last",
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

    tagcloud: function() {
        $.getJSON('/tags', function(data) {
            if(data) {
                new App.Views.TagCloud({ tags : data });
            } else {
                new Error({ message: "Error loading images." });
            }
        });
    },

    prev: function(page) {
        $.getJSON('/prev/'+page, function(data) {
            if(data) {
                var images = _(data).map(function(i) { return new Image(i); });
                new App.Views.Index({ images: images.reverse() });
            } else {
                new Error({ message: "Error loading images." });
                window.location.hash = '#';
            }
        });
    },

    next: function(page) {
        $.getJSON('/next/'+page, function(data) {
            if(data) {
                var images = _(data).map(function(i) { return new Image(i); });
                new App.Views.Index({ images: images});
            } else {
                new Error({ message: "Error loading images." });
                window.location.hash = '#';
            }
        });
    },

    first: function() {
        $.getJSON('/first', function(data) {
            if(data) {
                var images = _(data).map(function(i) { return new Image(i); });
                new App.Views.Index({ images: images});
            } else {
                new Error({ message: "Error loading images." });
                window.location.hash = '#';
            }
        });
    },

    last: function() {
        $.getJSON('/last', function(data) {
            if(data) {
                var images = _(data).map(function(i) { return new Image(i); });
                new App.Views.Index({ images: images.reverse()});
            } else {
                new Error({ message: "Error loading images." });
                window.location.hash = '#';
            }
        });
    },

    newDoc: function() {
        new App.Views.Edit({ model: new Image() });
    }
});

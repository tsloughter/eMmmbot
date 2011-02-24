var App = {
    Views: {},
    Controllers: {},
    Collections: {},
    init: function() {
        new App.Controllers.Images();
        Backbone.history.start();
    }
};

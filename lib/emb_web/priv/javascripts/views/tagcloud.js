App.Views.TagCloud = Backbone.View.extend({
    initialize: function() {
        this.tags = this.options.tags;
        this.render();
    },

    render: function() {
        var out = "<div id='wordcloud' style='width: 550px; height: 350px; position: relative;'></div>";
        $(this.el).html(out);
        $('#app').html(this.el);

        $("#wordcloud").jQCloud(this.tags);
    }
});
var Image = Backbone.Model.extend({
    url : function() {
      var base = 'images';
      if (this.isNew()) return base;
      return base + (base.charAt(base.length - 1) == '/' ? '' : '/') + this.id;
    }
});
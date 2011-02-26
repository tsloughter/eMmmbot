App.Views.Index = Backbone.View.extend({
    initialize: function() {
        this.images = this.options.images;
        this.render();
    },

    render: function() {
        if(this.images.length > 0) {
            var out = "<a href='/#first'>First</a> ";
            out += "<a href='/#prev/"+this.images[0].id+"'>Prev</a> ";
            out += " <a href='/#next/"+this.images[this.images.length-1].id+"'>Next</a> ";
            out += "<a href='/#last'>Last</a> ";
            out += "<div class='container'>";
            out += "<ul class='thumb'>";

            _(this.images).each(function(item) {
                out += "<li><a href='" + item.get('fullsize') + "'><img src='" + item.get('thumbnail') + "' /></a></li>";
            });
            out += "</ul>";
            out += "<div id='main_view'>";
	        out += "<a href='"+this.images[0].get('fullsize')+"' title='' target='_blank'><img src='"+this.images[0].get('fullsize')+"' alt=\"\" /></a><br />";
            out += "<div>";
			out += "<input type='text' name='newtag1' id='newtag1' style='font-size: 25px; width:225px; margin: 0 10px 8px 0;' />";
			out += "<button type='button' id='btnTagAdd1' style='margin: 10px 0 0 0;'>Add Tag</button>";
			out += "<div id='tagchecklist1' class='tagchecklist'>";
			out += "<strong>Current tags:</strong>";
   			out += "<br />";
            out += "</div>";
			out += "</div>";
            out += "</div>";
		    out += "<div style='clear:left'></div>";

            out += "</div>";
        } else {
            out = "<h3>No images! <a href='#new'>Create one</a></h3>";
        }

        $(this.el).html(out);
        $('#app').html(this.el);

        $("ul.thumb li").hover(function() {
	                               $(this).css({'z-index' : '10'});
	                               $(this).find('img').addClass("hover").stop()
		                               .animate({
			                                        marginTop: '-110px',
			                                        marginLeft: '-110px',
			                                        top: '50%',
			                                        left: '50%',
			                                        width: '174px',
			                                        height: '174px',
			                                        padding: '20px'
		                                        }, 200);

	                           } , function() {
	                               $(this).css({'z-index' : '0'});
	                               $(this).find('img').removeClass("hover").stop()
		                               .animate({
			                                        marginTop: '0',
			                                        marginLeft: '0',
			                                        top: '0',
			                                        left: '0',
			                                        width: '100px',
			                                        height: '100px',
			                                        padding: '5px'
		                                        }, 400);
                               });

        //Swap Image on Click
	    $("ul.thumb li a").click(function() {
		                             var mainImage = $(this).attr("href"); //Find Image Name
                                     $("#main_view a").attr({ href: mainImage });
		                             $("#main_view img").attr({ src: mainImage });
		                             return false;
	                             });

        $('#btnTagAdd1').click(function(){
		                           $('#newtag1').addtag({
			                                                holder_id : 'tagchecklist1',
			                                                prefix: 't1'
		                                                });
		                           $('#newtag1').val('');
	                           });
	    $('#newtag1').keydown(function(event){
		                          if ($.isenter({
			                                        event: event
		                                        })) {
			                          $('#newtag1').addtag({
				                                               holder_id: 'tagchecklist1',
				                                               prefix: 't1'
			                                               });
			                          $('#newtag1').val('');
			                          return false;
		                          }
	                          });
	    $('#newtag1').keypress(function(event){
		                           if ($.isenter({
			                                         event: event
		                                         })) {
			                           $('#newtag1').addtag({
				                                                holder_id: 'tagchecklist1',
				                                                prefix: 't1'
			                                                });
			                           $('#newtag1').val('');
			                           return false;
		                           }
	                           });
	    $('a.delbutton').click(function(e) {
		                           e.preventDefault();
		                           $(this).parent().remove();
	                           });

    }
});

// Remove "Last edited by" messaging
$("#last_edited_by").contents(':not(a)').remove();

// Hide/Show right-hand navigation
$(document).ready(function() {
    var rightnav_changed = false;

    function hide_right() {
        $("#hide_right").hide();
        setTimeout(function() {
            $("body").removeClass("with-right-side")
        }, 750);
        $("#right-side-wrapper").fadeOut("slow");
        $("#show_right").show();
		document.cookie = 'rightnav=hide; path=/'
        rightnav_changed = true;
    }

    function show_right() {
        setTimeout(function() {
            $("#right-side-wrapper").fadeIn("slow")
        }, 200);
        $("body").addClass("with-right-side");
        $("#show_right").hide();
        $("#hide_right").show();
		document.cookie = 'rightnav=show; path=/'
        rightnav_changed = true;
    }

    // Force refresh if cookie was updated and user navigates somewhere else
    $('.section a').on('click', function() {
        if (rightnav_changed) {
            console.log("Forcing refresh...");
            window.location.replace($(this).attr('href'));
        }
    });

    if ($("body").hasClass("with-right-side")) {
        // Add hide/show buttons to DOM
        $("#menu").prepend('<div class="pull-right unstyled" align="right"><button style="margin-right: 12px; margin-top: 5px; display:none;" class="btn btn-small" id="hide_right" ><i class="icon-arrow-right"></i>&nbspHide Right</button></div>');
        $("#menu").prepend('<div class="pull-right unstyled" align="right"><button style="margin-right: 12px; margin-top: 5px; display:none;" class="btn btn-small" id="show_right" ><i class="icon-arrow-left"></i>&nbspShow Right</button></div>');

        // Has the rightnav cookie been set to hide?
		if (window.document.cookie.indexOf('rightnav=hide') > -1) {
			$('#show_right').show();
			$("body").removeClass("with-right-side")				
		} else {
			$("#hide_right").show();
			document.cookie = 'rightnav=show; path=/'
		}

        // If hide is clicked, set rightnav cookie to hide
        $("#hide_right").on("click", function() {
            hide_right()
        });
        // If show is clicked, set rightnav cookie to show
        $("#show_right").on("click", function() {
            show_right()
        });
    }
});
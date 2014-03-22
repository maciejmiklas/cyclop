function initButtons() {

	// clqHelpExpandSupportBody
	$(".cq-clqHelpExpandSupportBody").hide();
	$(".cq-CqlHelpButton").click(function () {
		var $bodyToHide = $(".cq-clqHelpExpandSupportBody");
		var $button = $(".cq-CqlHelpButton");

		if ($bodyToHide.is(":hidden")) {
			$bodyToHide.slideDown("fast");
			$button.addClass("active");
		} else {
			$bodyToHide.slideUp("fast");
			$button.removeClass("active");
		}
	});

	// clqExecuteButton
	$(".cq-ExecuteQueryButton").click(function () {
		$(".cq-queryProgressBar").show();
		$(".cq-queryResult").hide();
		$(".cq-cqlInfoHint").hide();
		$(".cq-ExecuteQueryButton").addClass("disabled");
	});

	// cq-BookmarkButton
	$(".cq-BookmarkButton").click(function () {
			var $area = $(".cq-textareaCql");
			var text = $area.val();
			if (text.isEmpty()) {
				notify("CQL Editor is empty - there is nothing to bookmark ;)");
				return;
			}

			var encoded = encodeURIComponent(text);
			var url = $(location).attr('href');

			var lastHash = url.lastIndexOf("#");
			if (lastHash > 0) {
				url = url.substring(0, lastHash);
			}

			var urlBookmark;
			var qmIndex = url.lastIndexOf("?");
			if (qmIndex > 0) {
				urlBookmark = url.substr(0, qmIndex + 1) + "cql=" + encoded;

			} else {
				urlBookmark = url + "?cql=" + encoded;
			}

			window.history.replaceState({}, "cql", urlBookmark);

			notify("Page address in browser has been updated - now you can bookmark it :)");
		}
	)
}

function cqlQuerySuccessResponse() {
	$(".cq-queryProgressBar").hide();
	$(".cq-ExecuteQueryButton").removeClass("disabled");
}
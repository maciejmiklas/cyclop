function initButtons() {

	// clqHelpExpandSupportBody
	$(".cq-clqHelpExpandSupportBody").hide();
	$(".cq-clqHelpExpandSupportLink").click(function () {
		var $bodyToHide = $(".cq-clqHelpExpandSupportBody");
		var $button = $(".cq-clqHelpExpandSupportLink");

		if ($bodyToHide.is(":hidden")) {
			$bodyToHide.slideDown("fast");
			$button.addClass("active");
		} else {
			$bodyToHide.slideUp("fast");
			$button.removeClass("active");
		}
	});

	// clqExecuteButton
	$(".cq-clqExecuteButton").click(function () {
		$(".cq-queryProgressBar").show();
		$(".cq-queryResult").hide();
		$(".cq-clqExecuteButton").addClass("disabled");
	});

	// cq-clqBookmark
	$(".cq-clqBookmark").click(function () {
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

			notify("Page Address in browser has been updated - now you can bookmark it :)");
		}
	)
}

function cqlQuerySuccessResponse() {
	$(".cq-queryProgressBar").hide();
	$(".cq-clqExecuteButton").removeClass("disabled");
}
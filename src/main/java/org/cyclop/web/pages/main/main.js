var historyCallbackLink;

function initTabCallback(cssComponentId, link) {
	historyCallbackLink = link;

	$(cssComponentId).on("click", function () {
		Wicket.Ajax.get({
			"u": historyCallbackLink
		});
		console.log("TAB CLICK:" + historyCallbackLink);
	});
}
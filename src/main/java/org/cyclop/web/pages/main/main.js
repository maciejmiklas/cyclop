function initCallbackTab(cssComponentId, link, removeContentArray) {
	historyCallbackLink = link;

	$(cssComponentId).on("click", function () {
		Wicket.Ajax.get({
			"u": historyCallbackLink
		});
	});
};

function initStaticTab(removeContentArray) {

}
function initReloadableTab(tabLinkCssRef, reloacCallbackLink, removeContentArray) {
	$(tabLinkCssRef).on("click", function () {
		Wicket.Ajax.get({
			"u": reloacCallbackLink
		});
		removeContent(removeContentArray);
	});
};

function initStaticTab(tabLinkCssRef, removeContentArray) {
	$(tabLinkCssRef).on("click", function () {
		removeContent(removeContentArray);
	});
}

function removeContent(removeContentArray) {
	removeContentArray.map(function (item) {
		$(item).empty();
	});
}
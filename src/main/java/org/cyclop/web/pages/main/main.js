var historyCallbackLink;
var historyComponent;

function initHistoryCallback(link, component) {
	historyCallbackLink = link;
	historyComponent = component;

	$(".cq-tabHistory").click(function () {
		//Wicket.Ajax.ajax({"u":historyCallbackLink,"c":historyComponent});
		//Wicket.Ajax.ajax({"u":"./ced?1-0.IBehaviorListener.0-historyPanel","c":"id2"});
		console.log(historyCallbackLink + " - " + historyComponent);
	});
}
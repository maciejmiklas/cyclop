function notify(text) {
	var $notify = $("#cq-notify");

	$("div.notify_text").replaceWith("<div class='notify_text'>" + text + "</div>");

	$notify.toggleClass('in');

	setTimeout(function() {
		$('#cq-notify').removeClass('in');
	}, 5000);
}

String.prototype.isEmpty = function isEmpty() {
	return 0 === this.length;
};

String.prototype.endsWith = function(suffix) {
	return this.indexOf(suffix, this.length - suffix.length) !== -1;
};

function strcmp(a, b) {
	if (!a || !b) {
		return -1;
	}
	if (a.toString() < b.toString()) {
		return -1;
	}
	if (a.toString() > b.toString()) {
		return 1;
	}
	return 0;
}
function streq(a, b) {
	if (!a || !b) {
		return false;
	}
	if (a.toString() < b.toString() || a.toString() > b.toString()) {
		return false;
	}
	return true;
}

function notify(text) {
    var $notify = $("#cq-notify");

    $("div.notify_text").replaceWith("<div class='notify_text'>" + text + "</div>");

    $notify.toggleClass('in');

    setTimeout(function () {
        $('#cq-notify').removeClass('in');
    }, 5000);
}

String.prototype.isEmpty = function isEmpty() {
    return 0 === this.length;
};

String.prototype.endsWith = function (suffix) {
    return this.indexOf(suffix, this.length - suffix.length) !== -1;
};
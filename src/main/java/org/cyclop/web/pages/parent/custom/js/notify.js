function notify(text) {
    var $notify = $("#cq-notify");

    $("div.notify_text").replaceWith("<div class='notify_text'>" + text + "</div>");

    $notify.toggleClass('in');

    setTimeout(function () {
        $('#cq-notify').removeClass('in');
    }, 5000);
}

function isEmpty(str) {
    return 0 === str.length;
}
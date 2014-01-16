$(function () {
    var $dialog = $("${infoDialogId}");
    $dialog.dialog({
        autoOpen: false,
        width: 640,
        maxHeight: 640,
        show: {
            effect: "blind",
            duration: 150
        },
        hide: {
            effect: "blind",
            duration: 150
        },
        close: function () {
            $dialog.dialog("destroy");
            $dialog.css('display', 'none')
        }
    });
    $dialog.dialog("open");
});



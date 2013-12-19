$(function () {
    $("${infoDialogId}").dialog({
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
            $(this).dialog("destroy");
            $(this).css('display', 'none')
        }
    });

    $("${infoDialogId}").dialog("open");
});



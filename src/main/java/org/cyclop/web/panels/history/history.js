$(function () {
	initResetButton();
});

function initResetButton() {
	var $input = $(".cq-historyFilterField");
	$input.keypress(function (e) {
		switch (e.keyCode) {
			case 13://ENTER
				$(".cq-ResetHistoryFilter").click();
		}
	});

}
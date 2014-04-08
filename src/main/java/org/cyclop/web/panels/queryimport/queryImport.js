var scriptFileName = null;

$(function () {
	$(".cq-executeQueryImportButton").click(function () {
		if (!scriptFileName) {
			notify("Please select Script File first");
		}
		else {
			$(".cq-queryImportProgressBar").show();
			$(".cq-executeQueryImportButton").addClass("disabled");
		}
	});

});

function onQueryImportResponse() {
	$(".cq-queryImportProgressBar").hide();
	$(".cq-executeQueryImportButton").removeClass("disabled");
}

$(document).on('change', '.cq-btn-file :file', function () {
	var $input = $(this);
	var $label = $input.parents('.cq-btn-file').find('.cq-selected-file-name');
	scriptFileName = $input.val();
	var labelVal = scriptFileName.replace(/\\/g, '/').replace(/.*\//, '');
	$label.text("File: " + labelVal);
});

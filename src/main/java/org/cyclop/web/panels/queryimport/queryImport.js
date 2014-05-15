/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
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

function onQueryImportResponse(response) {
	$(".cq-queryImportProgressBar").hide();
	$(".cq-executeQueryImportButton").removeClass("disabled");
	if (streq(response, "FILE_SIZE_LIMIT")) {
		notify("Import file is to large");
	}
	else if (response) {
		notify(response);
	}
}

$(document).on('change', '.cq-btn-file :file', function () {
	var $input = $(this);
	var $label = $input.parents('.cq-btn-file').find('.cq-selected-file-name');
	scriptFileName = $input.val();
	var labelVal = scriptFileName.replace(/\\/g, '/').replace(/.*\//, '');
	$label.text("File: " + labelVal);
});

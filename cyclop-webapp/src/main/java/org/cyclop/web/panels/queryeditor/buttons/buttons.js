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
$(function () {

	// clqHelpExpandSupportBody
	$(".cq-clqHelp-expandSupportBody").hide();
	$(".cq-CqlHelpButton").click(function () {
		var $bodyToHide = $(".cq-clqHelp-expandSupportBody");
		var $button = $(".cq-CqlHelpButton");

		if ($bodyToHide.is(":hidden")) {
			$bodyToHide.slideDown("fast");
			$button.addClass("active");
		} else {
			$bodyToHide.slideUp("fast");
			$button.removeClass("active");
		}
	});

	// clqExecuteButton
	$(".cq-ExecuteQueryButton").click(function () {
		$(".cq-queryProgressBar").show();
		$(".cq-queryResult").hide();
		$(".cq-queryHint-dialog").hide();
		$(".cq-ExecuteQueryButton").addClass("disabled");
	});

	// cq-BookmarkButton
	$(".cq-BookmarkButton").click(function () {
			var $area = $(".cq-cqledit");
			var text = $area.val();
			if (text.isEmpty()) {
				notify("CQL Editor is empty - there is nothing to bookmark ;)");
				return;
			}

			var encoded = encodeURIComponent(text);
			var url = $(location).attr('href');

			var lastHash = url.lastIndexOf("#");
			if (lastHash > 0) {
				url = url.substring(0, lastHash);
			}

			var urlBookmark;
			var qmIndex = url.lastIndexOf("?");
			if (qmIndex > 0) {
				urlBookmark = url.substr(0, qmIndex + 1) + "cql=" + encoded;

			} else {
				urlBookmark = url + "?cql=" + encoded;
			}

			window.history.replaceState({}, "cql", urlBookmark);

			notify("Page address in browser has been updated - now you can bookmark it :)");
		}
	);
});

function queryExecutedResponse() {
	$(".cq-queryProgressBar").hide();
	$(".cq-ExecuteQueryButton").removeClass("disabled");
}
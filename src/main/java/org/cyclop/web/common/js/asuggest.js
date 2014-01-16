/*
 /*
 * jQuery textarea suggest plugin
 *
 * Copyright (c) 2009-2010 Roman Imankulov
 *
 * Dual licensed under the MIT and GPL licenses:
 *   http://www.opensource.org/licenses/mit-license.php
 *   http://www.gnu.org/licenses/gpl.html
 *
 * Requires:
 *   - jQuery (tested with 1.3.x and 1.4.x)
 *   - jquery.a-tools >= 1.4.1 (http://plugins.jquery.com/project/a-tools)
 */

function getCaretPosition(ctrl) {
    var CaretPos = 0;	// IE Support
    if (document.selection) {
        ctrl.focus();
        var sel = document.selection.createRange();
        sel.moveStart('character', -ctrl.value.length);
        CaretPos = sel.text.length;
    }
    // Firefox support
    else if (ctrl.selectionStart || ctrl.selectionStart == '0')
        CaretPos = ctrl.selectionStart;
    return (CaretPos);
}

function initSuggests(editorId, suggests) {

    String.prototype.endsWith = function (suffix) {
        return this.indexOf(suffix, this.length - suffix.length) !== -1;
    };

    window.suggests = suggests;
    var $editor = $(editorId);
    $editor.asuggest({
        'endingSymbols': '',
        'stopSuggestionKeys': [$.asuggestKeys.RETURN],
        'minChunkSize': 1,
        'delimiters': ', ={}():'
    });
}

function replaceSuggests(editorId, suggests) {
    window.suggests = suggests;
}

(function ($) {
    // workaround for Opera browser
    if (navigator.userAgent.match(/opera/i)) {
        $(document).keypress(function (e) {
            if ($.asuggestFocused) {
                $.asuggestFocused.focus();
                $.asuggestFocused = null;
                e.preventDefault();
                e.stopPropagation();
            }
        });
    }

    $.asuggestKeys = {
        UNKNOWN: 0,
        SHIFT: 16,
        CTRL: 17,
        ALT: 18,
        LEFT: 37,
        UP: 38,
        RIGHT: 39,
        DOWN: 40,
        DEL: 46,
        TAB: 9,
        RETURN: 13,
        ESC: 27,
        COMMA: 188,
        PAGEUP: 33,
        PAGEDOWN: 34,
        BACKSPACE: 8,
        SPACE: 32
    };
    $.asuggestFocused = null;

    $.fn.asuggest = function (options) {
        return this.each(function () {
            $.makeSuggest(this, options);
        });
    };

    $.fn.asuggest.defaults = {
        'delimiters': '\n ',
        'minChunkSize': 1,
        'endingSymbols': ' ',
        'stopSuggestionKeys': [$.asuggestKeys.RETURN, $.asuggestKeys.SPACE]
    };

    /* Make suggest:
     *
     * create and return jQuery object on the top of DOM object
     * and store suggests as part of this object
     *
     * @param area: HTML DOM element to add suggests to
     * @param suggests: The array of suggest strings
     * @param options: The options object
     */
    $.makeSuggest = function (area, options) {
        options = $.extend({}, $.fn.asuggest.defaults, options);
        var KEY = $.asuggestKeys;
        var $area = $(area);
        $area.options = options;
        $area.lastSuggest = "";
        $area.lastText = "";

        // get the chunk of text (separated by delimiter) before the cursor
        $area.getSuggestChunkBeforeCursor = function () {
            var delimiters = $area.options.delimiters.split(''); // array of chars
            var textBeforeCursor = $area.getTextBeforeCursor();
            var indexOfDelimiter = -1;
            var idx;
            for (var i = 0; i < delimiters.length; i++) {
                var d = delimiters[i];
                idx = textBeforeCursor.lastIndexOf(d);
                if (idx > indexOfDelimiter) {
                    indexOfDelimiter = idx;
                }
            }

            var chunk;
            if (indexOfDelimiter < 0) {
                chunk = textBeforeCursor;
            } else {
                chunk = textBeforeCursor.substr(indexOfDelimiter + 1);
            }
            return chunk;
        };

        $area.endsWithLastSuggest = function () {
            var str = $area.lastText;
            var endText = $area.lastSuggest;
            if (!str || !endText) {
                return false;
            }
            return str.indexOf(endText, str.length - endText.length) !== -1;
        };

        $area.getTextBeforeCursor = function () {
            return $area.val().substr(0, $area.getSelection().start).toLowerCase();
        };

        /** support for suggests of keywords containing space like "drop table" */
        $area.getCompletionForLastSuggest = function () {

            var suggestFromLast = null;

            if ($area.endsWithLastSuggest()) {
                var matchIdx = $area.lastSuggest.length;
                var textBeforeCursor = $area.getTextBeforeCursor().toLowerCase();
                var suggestLow = $area.lastSuggest.toLowerCase();
                while (matchIdx >= $area.options.minChunkSize) {
                    var endsWithText = suggestLow.substr(0, matchIdx);
                    if (textBeforeCursor.endsWith(endsWithText)) {
                        suggestFromLast = $area.lastSuggest.substr(matchIdx, $area.lastSuggest.length);
                        break;
                    }
                    matchIdx--;
                }
            }
            return suggestFromLast;
        };

        /** completion when user types text (inputs next characters into textarea) */
        $area.getCompletionForInput = function () {

            var suggestFromLast = $area.getCompletionForLastSuggest();
            if (suggestFromLast) {
                return suggestFromLast;
            }

            var chunk = $area.getSuggestChunkBeforeCursor();
            if (chunk.length < $area.options.minChunkSize) {
                return null;
            }

            var foundCompletion = null;
            for (var i = 0; i < window.suggests.length; i++) {
                var suggest = window.suggests[i].toLowerCase();
                // some suggest is found
                if (suggest.indexOf(chunk) === 0) {
                    foundCompletion = suggest.substr(chunk.length);
                    $area.lastSuggest = suggest;
                }
            }

            return foundCompletion;
        };

        /** completion for pressing TAB - cycles trough possible values */
        $area.getCompletionForCycle = function () {
            var chunk = $area.getSuggestChunkBeforeCursor().toLowerCase();
            var selectionText = $area.getSelection().text.toLowerCase();
            var areaText = chunk + selectionText;

            var foundCompletion = null;
            var firstMatchedValue = null;
            var foundAlreadySelectedValue = false;
            for (var i = 0; i < window.suggests.length; i++) {
                var suggest = window.suggests[i].toLowerCase();
                // some suggest is found
                if (suggest.indexOf(chunk) === 0) {
                    $area.lastSuggest = suggest;

                    if (areaText === suggest) {
                        foundAlreadySelectedValue = true;

                    } else if (foundAlreadySelectedValue) {
                        foundCompletion = suggest.substr(chunk.length);

                    } else if (firstMatchedValue === null) {
                        firstMatchedValue = suggest;
                    }
                }
            }
            if (foundCompletion === null && firstMatchedValue) {
                foundCompletion = firstMatchedValue.substr(chunk.length);
            }

            return foundCompletion;
        };

        $area.isInputUpperCase = function () {
            // TODO simplify it - to not call getSuggestChunkBeforeCursor()
            var selectionText = $area.getSuggestChunkBeforeCursor();
            return selectionText.toUpperCase() === selectionText && selectionText.toLowerCase() !== selectionText;
        };

        /**
         * @param completionText completion text after cursor (the highlighted part)
         */
        $area.updateCompletion = function (completionText) {
            if (!completionText) {
                return;
            }
            if ($area.isInputUpperCase()) {
                completionText = completionText.toUpperCase();
            }
            var selectionStart = $area.getSelection().start;
            var selectionEnd = selectionStart + completionText.length;
            if ($area.getSelection().text === "") {
                if ($area.val().length === selectionStart) { // IE workaround
                    $area.setCaretPos(selectionStart + 10000);
                }
                $area.insertAtCaretPos(completionText);
            } else {
                $area.replaceSelection(completionText);
            }
            $area.setSelection(selectionStart, selectionEnd);
        };

        $area.keydown(function (e) {
            if (e.keyCode === KEY.TAB) {
                var chunk = $area.getSuggestChunkBeforeCursor();
                if (chunk.length >= $area.options.minChunkSize) {
                    var completionText = $area.getCompletionForCycle();
                    $area.updateCompletion(completionText);
                }
                e.preventDefault();
                e.stopPropagation();
                $area.focus();
                $.asuggestFocused = this;
                return false;
            }
            // Check for conditions to stop suggestion
            if ($area.getSelection().length &&
                $.inArray(e.keyCode, $area.options.stopSuggestionKeys) !== -1) {
                // apply suggestion. Clean up selection and insert a space
                var selectionEnd = $area.getSelection().end + $area.options.endingSymbols.length;
                var text = $area.getSelection().text + $area.options.endingSymbols;
                $area.replaceSelection(text);
                $area.setSelection(selectionEnd, selectionEnd);
                e.preventDefault();
                e.stopPropagation();
                this.focus();
                $.asuggestFocused = this;

                // fire onchange on document in order to trigger ajax events
                $area.change();
                return false;
            }
            return true;
        });

        $area.keyup(function (e) {
            var hasSpecialKeys = e.altKey || e.metaKey || e.ctrlKey,
                hasSpecialKeysOrShift = hasSpecialKeys || e.shiftKey;
            switch (e.keyCode) {
                case KEY.UNKNOWN: // Special key released
                case KEY.SHIFT:
                case KEY.CTRL:
                case KEY.ALT:
                case KEY.RETURN:
                    if (e.ctrlKey) {
                        $(".cq-clqExecuteButton").click();
                    }
                    break;
                case KEY.TAB:
                    if (!hasSpecialKeysOrShift) {
                        break;
                    }
                case KEY.ESC:
                case KEY.BACKSPACE:
                case KEY.DEL:
                case KEY.UP:
                case KEY.DOWN:
                case KEY.LEFT:
                case KEY.RIGHT:
                    if (!hasSpecialKeysOrShift) {
                        $area.replaceSelection("");
                    }
                    break;
                default:
                    if (!hasSpecialKeys) {
                        var completion = $area.getCompletionForInput();
                        $area.updateCompletion(completion);
                    }
                    break;
            }
            $area.lastText = $area.val();
        });
        return $area;
    };
}(jQuery));

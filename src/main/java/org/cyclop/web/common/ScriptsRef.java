package org.cyclop.web.common;

import org.apache.wicket.request.resource.JavaScriptResourceReference;
import org.cyclop.web.resources.ResRef;

/** @author Maciej Miklas */
public final class ScriptsRef {

	public static final JavaScriptResourceReference JQUERY_TOOLS = new JavaScriptResourceReference(ResRef.class,
			"js/jquery.a-tools.js");

	public static final JavaScriptResourceReference SUGGEST = new JavaScriptResourceReference(ResRef.class,
			"js/asuggest.js");

	public static final JavaScriptResourceReference COMMON = new JavaScriptResourceReference(ResRef.class,
			"js/common.js");

	public static final JavaScriptResourceReference BOOTSTRAP = new JavaScriptResourceReference(ResRef.class,
			"bootstrap/js/bootstrap.js");

	public static final JavaScriptResourceReference JQUERY_UI = new JavaScriptResourceReference(ResRef.class,
			"jqueryui/js/jquery-ui.js");
}

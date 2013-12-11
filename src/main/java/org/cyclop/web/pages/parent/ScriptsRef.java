package org.cyclop.web.pages.parent;

import org.apache.wicket.request.resource.JavaScriptResourceReference;

/**
 * @author Maciej Miklas
 */
public final class ScriptsRef {

    public static final JavaScriptResourceReference JQUERY_TOOLS = new JavaScriptResourceReference(ScriptsRef.class,
            "custom/js/jquery.a-tools.js");

    public static final JavaScriptResourceReference SUGGEST = new JavaScriptResourceReference(ScriptsRef.class,
            "custom/js/asuggest.js");

    public static final JavaScriptResourceReference NOTIFY = new JavaScriptResourceReference(ScriptsRef.class,
            "custom/js/notify.js");

    public static final JavaScriptResourceReference BOOTSTRAP = new JavaScriptResourceReference(ScriptsRef.class,
            "bootstrap/js/bootstrap.js");

    public static final JavaScriptResourceReference JQUERY_UI = new JavaScriptResourceReference(ScriptsRef.class,
            "jqueryui/js/jquery-ui.js");
}

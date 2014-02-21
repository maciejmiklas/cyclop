package org.cyclop.web.common;

public interface AjaxRefreshSupport {

	/** @return ajax url which will trigger reload of this component */
	String getRefreshContentCallbackUrl();

	/** @return CSS class used by jquery to find link to register content-refresh-callback-ajax-request */
	String getRefreshLinkCssClass();

	/**
	 * @return CSS class name used to find content of this component. This content will be removed in browser when tab
	 *         containing this component is not visible anymore. Main reason to do that is performance improvements - we
	 *         try to keep dom tree as small as possible
	 */
	String getContentCssClass();

}

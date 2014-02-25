package org.cyclop.web.common;

/** @author Maciej Miklas */
public interface AjaxReloadSupport {

	/** @return ajax link that can be used to trigger reload of this component (or part of it). */
	String getReloadCallbackUrl();

	/**
	 * @return CSS class name used to find ajax-reloadable-content of this component. Content of this tag will be
	 *         removed in browser, when user switches to another tab. Main reason to do that is performance improvements
	 *         - we try to keep dom tree as small as possible
	 */
	String getReloadableContentCssRef();

}

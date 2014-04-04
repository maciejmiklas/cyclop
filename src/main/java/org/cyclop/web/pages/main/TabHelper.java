package org.cyclop.web.pages.main;

import org.apache.wicket.markup.head.IHeaderResponse;
import org.apache.wicket.markup.head.OnDomReadyHeaderItem;
import org.cyclop.web.common.AjaxReloadSupport;
import org.cyclop.web.common.JsFunctionBuilder;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

class TabHelper {

	private final List<FullRefreshData> refreshTabs = new ArrayList<>();

	private final List<String> removableContentCssRefs = new ArrayList<>();

	/** CSS class name that can be used to find link activating static tab */
	private final List<String> staticTabLinkCssRefs = new ArrayList<>();

	/**
	 * Registers tab which content will be reloaded when user selects this tab.
	 *
	 * @param tabClikcLinkCssRef
	 * 		CSS class used by jquery to find link to register content-refresh-callback-ajax-request
	 */
	public void registerReloadableTab(AjaxReloadSupport tab, String tabClikcLinkCssRef) {
		refreshTabs.add(new FullRefreshData(tab, tabClikcLinkCssRef));
		removableContentCssRefs.add(tab.getRemovableContentCssRef());
	}

	/**
	 * Registers tab which content will not be reloaded when user selects it.
	 *
	 * @param tabClikcLinkCssClass
	 * 		this class will be used by jquery to register on-click-handler. It will remove content of all other tabs that
	 * 		support ajax reload.
	 */
	public void registerSaticTab(String tabClikcLinkCssClass) {
		staticTabLinkCssRefs.add(tabClikcLinkCssClass);
	}

	public void renderHead(IHeaderResponse response) {
		for (FullRefreshData tab : refreshTabs) {
			initReloadableTab(response, tab);
		}

		for (String tab : staticTabLinkCssRefs) {
			initStaticTab(response, tab);
		}
	}

	private void initStaticTab(IHeaderResponse response, String staticTabLinkCssRef) {
		String callbackJs = JsFunctionBuilder.function("initStaticTab").param(staticTabLinkCssRef)
				.arrayStr(removableContentCssRefs).build();

		response.render(OnDomReadyHeaderItem.forScript(callbackJs));
	}

	private void initReloadableTab(IHeaderResponse response, FullRefreshData data) {
		Set<String> removeContentList = generateRemoveContentList(data);

		String callbackJs = JsFunctionBuilder.function("initReloadableTab").param(data.tabClikcLinkCssRef)
				.param(data.panel.getReloadCallbackUrl()).arrayStr(removeContentList).build();

		response.render(OnDomReadyHeaderItem.forScript(callbackJs));
	}

	private Set<String> generateRemoveContentList(FullRefreshData data) {
		Set<String> removeContentList = new HashSet<>(removableContentCssRefs);
		removeContentList.remove(data.panel.getRemovableContentCssRef());
		return removeContentList;
	}

	private class FullRefreshData {
		private final AjaxReloadSupport panel;

		private final String tabClikcLinkCssRef;

		public FullRefreshData(AjaxReloadSupport panel, String tabClikcLinkCssRef) {
			this.panel = panel;
			this.tabClikcLinkCssRef = tabClikcLinkCssRef;
		}
	}

}

package org.cyclop.web.pages.main;

import org.apache.wicket.markup.head.IHeaderResponse;
import org.apache.wicket.markup.head.OnDomReadyHeaderItem;
import org.apache.wicket.markup.html.panel.Panel;
import org.cyclop.web.common.AjaxRefreshSupport;
import org.cyclop.web.common.JsFunctionBuilder;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

class TabSupport {

	private final List<Panel> tabs = new ArrayList<>();

	private final List<String> contentCssClasses = new ArrayList<>();

	public void registerTab(Panel tab) {
		tabs.add(tab);

		if (tab instanceof AjaxRefreshSupport) {
			AjaxRefreshSupport atab = (AjaxRefreshSupport) tab;
			contentCssClasses.add(atab.getContentCssClass());
		}

	}

	public void renderHead(IHeaderResponse response) {
		for (Panel tab : tabs) {
			if (tab instanceof AjaxRefreshSupport) {
				initCallbackTab(response, (AjaxRefreshSupport) tab);
			} else {
				initStaticTab(response);
			}
		}
	}

	private void initStaticTab(IHeaderResponse response) {
		String callbackJs = JsFunctionBuilder.function("initStaticTab").arrayStr(contentCssClasses).build();

		response.render(OnDomReadyHeaderItem.forScript(callbackJs));
	}

	private void initCallbackTab(IHeaderResponse response, AjaxRefreshSupport reloadSupport) {
		Set<String> removeContentList = generateRemoveContentList(reloadSupport);

		String callbackJs = JsFunctionBuilder.function("initCallbackTab").param(reloadSupport.getRefreshLinkCssClass())
				.param(reloadSupport.getRefreshContentCallbackUrl()).arrayStr(removeContentList).build();

		response.render(OnDomReadyHeaderItem.forScript(callbackJs));
	}

	private Set<String> generateRemoveContentList(AjaxRefreshSupport reloadSupport) {
		Set<String> removeContentList = new HashSet<>(contentCssClasses);
		removeContentList.remove(reloadSupport.getContentCssClass());
		return removeContentList;
	}

}

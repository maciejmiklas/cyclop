package org.cyclop.web.pages.main;

import org.apache.wicket.authroles.authorization.strategies.role.Roles;
import org.apache.wicket.authroles.authorization.strategies.role.annotations.AuthorizeInstantiation;
import org.apache.wicket.markup.head.IHeaderResponse;
import org.apache.wicket.markup.head.JavaScriptReferenceHeaderItem;
import org.apache.wicket.markup.head.OnDomReadyHeaderItem;
import org.apache.wicket.markup.html.link.Link;
import org.apache.wicket.request.mapper.parameter.PageParameters;
import org.apache.wicket.request.resource.JavaScriptResourceReference;
import org.cyclop.web.pages.authenticate.AuthenticatePage;
import org.cyclop.web.pages.parent.ParentPage;
import org.cyclop.web.panels.commander.CommanderPanel;
import org.cyclop.web.panels.history.HistoryPanel;

import static org.cyclop.web.common.JsUtils.escapeParam;

/** @author Maciej Miklas */
@AuthorizeInstantiation(Roles.ADMIN)
public class MainPage extends ParentPage {

	private static final JavaScriptResourceReference JS_MAIN = new JavaScriptResourceReference(MainPage.class,
			"main.js");

	private HistoryPanel historyPanel;

	public MainPage(PageParameters params) {
		initCommanderTab(params);
		historyPanel = initHistoryTab();
		initLogout();
	}

	private void initLogout() {
		Link<Void> logOut = new Link<Void>("logOut") {
			@Override
			public void onClick() {
				getSession().invalidate();
				setResponsePage(AuthenticatePage.class);
			}
		};
		add(logOut);
	}

	private void initCommanderTab(PageParameters params) {
		CommanderPanel commanderPanel = new CommanderPanel("commanderPanel", params);
		add(commanderPanel);
	}

	private HistoryPanel initHistoryTab() {
		HistoryPanel historyPanel = new HistoryPanel("historyPanel");
		add(historyPanel);
		return historyPanel;
	}

	@Override
	public void renderHead(IHeaderResponse response) {
		super.renderHead(response);
		response.render(JavaScriptReferenceHeaderItem.forReference(JS_MAIN));

		String initHistoryCallbackJs = generateInitCallback(".cq-tabHistory",
				historyPanel.getRefreshContentCallbackUrl());
		response.render(OnDomReadyHeaderItem.forScript(initHistoryCallbackJs));
	}

	private String generateInitCallback(String cssComponentId, String callbackLink) {
		StringBuilder buf = new StringBuilder("initTabCallback");
		buf.append("(");
		buf.append(escapeParam(cssComponentId));
		buf.append(",");
		buf.append(escapeParam(callbackLink));
		buf.append(")");
		return buf.toString();
	}
}

package org.cyclop.web.pages.main;

import org.apache.wicket.authroles.authorization.strategies.role.Roles;
import org.apache.wicket.authroles.authorization.strategies.role.annotations.AuthorizeInstantiation;
import org.apache.wicket.markup.head.IHeaderResponse;
import org.apache.wicket.markup.head.JavaScriptReferenceHeaderItem;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.link.Link;
import org.apache.wicket.request.mapper.parameter.PageParameters;
import org.apache.wicket.request.resource.JavaScriptResourceReference;
import org.cyclop.common.AppConfig;
import org.cyclop.web.pages.authenticate.AuthenticationPage;
import org.cyclop.web.pages.parent.ParentPage;
import org.cyclop.web.panels.about.AboutPanel;
import org.cyclop.web.panels.favourites.FavouritesPanel;
import org.cyclop.web.panels.history.HistoryPanel;
import org.cyclop.web.panels.queryeditor.QueryEditorPanel;
import org.cyclop.web.panels.queryimport.QueryImportPanel;

/** @author Maciej Miklas */
@AuthorizeInstantiation(Roles.ADMIN)
public class MainPage extends ParentPage {

	private final TabHelper tabSupport;

	private static final JavaScriptResourceReference JS_MAIN = new JavaScriptResourceReference(MainPage.class,
			"main.js");

	public MainPage(PageParameters params) {
		tabSupport = new TabHelper();
		initQueryEditorTab(params, tabSupport);
		initHistoryTab(tabSupport);
		// initFavouritesTab();
		initQueryImportTab(tabSupport);
		initAboutTab(tabSupport);
		initLogout();
	}

	private void initLogout() {
		Link<Void> logOut = new Link<Void>("logOut") {
			@Override
			public void onClick() {
				getSession().invalidate();
				setResponsePage(AuthenticationPage.class);
			}
		};
		add(logOut);
	}

	private void initQueryImportTab(TabHelper tabSupport) {
		QueryImportPanel queryImportPanel = new QueryImportPanel("queryImportPanel");
		add(queryImportPanel);
		tabSupport.registerSaticTab(".cq-tabQueryImport");
	}

	private void initQueryEditorTab(PageParameters params,TabHelper tabSupport) {
		QueryEditorPanel queryEditorPanel = new QueryEditorPanel("queryEditorPanel", params);
		add(queryEditorPanel);
		tabSupport.registerSaticTab(".cq-tabQueryEditor");
	}

	private void initFavouritesTab() {
		if (!AppConfig.get().favourites.enabled) {
			add(new Label("favouritesPanel", "Favourites are disabled in application-configuration"));
			return;
		}
		FavouritesPanel favourites = new FavouritesPanel("favouritesPanel");
		add(favourites);
		tabSupport.registerReloadableTab(favourites, ".cq-tabFavourites");
	}

	private void initAboutTab(TabHelper tabSupport) {
		AboutPanel panel = new AboutPanel("aboutPanel");
		add(panel);
		tabSupport.registerReloadableTab(panel, ".cq-tabAbout");
	}

	private void initHistoryTab(TabHelper tabSupport) {
		if (!AppConfig.get().history.enabled) {
			add(new Label("historyPanel", "History is disabled in application-configuration"));
			return;
		}
		HistoryPanel historyPanel = new HistoryPanel("historyPanel");
		add(historyPanel);
		tabSupport.registerReloadableTab(historyPanel, ".cq-tabHistory");
	}

	@Override
	public void renderHead(IHeaderResponse response) {
		super.renderHead(response);

		response.render(JavaScriptReferenceHeaderItem.forReference(JS_MAIN));
		tabSupport.renderHead(response);
	}

}

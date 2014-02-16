package org.cyclop.web.pages.main;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.AjaxFallbackLink;
import org.apache.wicket.authroles.authorization.strategies.role.Roles;
import org.apache.wicket.authroles.authorization.strategies.role.annotations.AuthorizeInstantiation;
import org.apache.wicket.request.mapper.parameter.PageParameters;
import org.cyclop.web.panels.commander.CqlCommanderPanel;
import org.cyclop.web.pages.parent.ParentPage;
import org.cyclop.web.panels.history.HistoryPanel;

/** @author Maciej Miklas */
@AuthorizeInstantiation(Roles.ADMIN)
public class MainPage extends ParentPage{

	public MainPage(PageParameters params) {
		CqlCommanderPanel cqlCommanderPanel = new CqlCommanderPanel("cqlCommanderPanel", params);
		add(cqlCommanderPanel);

		HistoryPanel historyPanel = new HistoryPanel("historyPanel");
		add(historyPanel);

		AjaxFallbackLink<Void> addToFavourites = new AjaxFallbackLink<Void>("tabHistoryList") {
			@Override
			public void onClick(AjaxRequestTarget target) {
				System.out.println("AAAAAAAAAAAAAAAAAAAAAAAAAA");
				//buttonListener.onClickExecCql(target);
			}
		};
		add(addToFavourites);
	}
}

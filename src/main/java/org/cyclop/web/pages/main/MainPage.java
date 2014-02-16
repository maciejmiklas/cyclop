package org.cyclop.web.pages.main;

import org.apache.wicket.authroles.authorization.strategies.role.Roles;
import org.apache.wicket.authroles.authorization.strategies.role.annotations.AuthorizeInstantiation;
import org.apache.wicket.request.mapper.parameter.PageParameters;
import org.cyclop.web.pages.cqlcommander.CqlCommanderPanel;
import org.cyclop.web.pages.parent.ParentPage;

/** @author Maciej Miklas */
@AuthorizeInstantiation(Roles.ADMIN)
public class MainPage extends ParentPage{

	public MainPage(PageParameters params) {
		CqlCommanderPanel cqlCommanderPanel = new CqlCommanderPanel("cqlCommanderPanel", params);
		add(cqlCommanderPanel);
	}
}

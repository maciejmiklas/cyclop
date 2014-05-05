package org.cyclop.web.pages.authenticate;

import org.cyclop.web.pages.parent.ParentPage;

/** @author Maciej Miklas */
public class AuthenticationPage extends ParentPage {

	public AuthenticationPage() {
		LoginPanel signInPanel = new LoginPanel("signInPanel");
		add(signInPanel);
	}
}

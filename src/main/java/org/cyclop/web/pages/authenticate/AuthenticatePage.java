package org.cyclop.web.pages.authenticate;

import org.cyclop.web.pages.parent.ParentPage;

/**
 * @author Maciej Miklas
 */
public class AuthenticatePage extends ParentPage {

    public AuthenticatePage() {
        LoginPanel signInPanel = new LoginPanel("signInPanel");
        add(signInPanel);
    }
}

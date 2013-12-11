package org.cyclop.web.pages.authenticate;

import org.apache.wicket.authroles.authentication.panel.SignInPanel;
import org.cyclop.web.pages.parent.ParentPage;

/**
 * @author Maciej Miklas
 */
public class AuthenticatePage extends ParentPage {

    public AuthenticatePage(){
        SignInPanel signInPanel = new SignInPanel("signInPanel");
        add(signInPanel);
    }
}

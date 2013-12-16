package org.cyclop.web.pages.authenticate;

import org.apache.wicket.authroles.authentication.panel.SignInPanel;
import org.cyclop.web.webapp.CqlWebSession;

/**
 * @author Maciej Miklas
 */
public class LoginPanel extends SignInPanel {
    public LoginPanel(String id) {
        super(id);
    }

    @Override
    protected void onSignInFailed() {
        CqlWebSession session = (CqlWebSession) getWebSession();
        if (session.getLastLoginError() != null) {
            error(session.getLastLoginError());
        } else {
            super.onSignInFailed();
        }
    }
}

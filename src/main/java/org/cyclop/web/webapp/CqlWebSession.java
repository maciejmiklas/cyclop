package org.cyclop.web.webapp;

import javax.inject.Inject;

import org.apache.wicket.authroles.authentication.AuthenticatedWebSession;
import org.apache.wicket.authroles.authorization.strategies.role.Roles;
import org.apache.wicket.injection.Injector;
import org.apache.wicket.request.Request;
import org.cyclop.service.cassandra.CassandraSession;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author Maciej Miklas
 */
public class CqlWebSession extends AuthenticatedWebSession {
    private final static Logger LOG = LoggerFactory.getLogger(CqlWebSession.class);

    @Inject
    private CassandraSession cassandraSession;

    private boolean authenticated = false;

    private String lastLoginError = null;

    public CqlWebSession(Request request) {
        super(request);
        Injector.get().inject(this);
    }

    @Override
    public boolean authenticate(String username, String password) {
        try {
            cassandraSession.authenticate(username, password);
            authenticated = true;
            lastLoginError = null;
        } catch (Exception e) {
            lastLoginError = e.getMessage();
            authenticated = false;

            LOG.info("Sing-in failed:" + e.getMessage(), e);
            LOG.debug(e.getMessage(), e);
        }

        return authenticated;
    }

    public String getLastLoginError() {
        return lastLoginError;
    }

    @Override
    public Roles getRoles() {
        return new Roles(authenticated ? Roles.ADMIN : "NO_AUTH");
    }

    @Override
    public void invalidate() {
        cassandraSession.close();
        super.invalidate();
    }
}

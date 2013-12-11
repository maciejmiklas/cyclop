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

    public CqlWebSession(Request request) {
        super(request);
        Injector.get().inject(this);
    }

    boolean authenticated = false;

    @Override
    public boolean authenticate(String username, String password) {
        try {
            cassandraSession.authenticate(username, password);
            authenticated = true;
        } catch (Exception e) {
            LOG.info("Sing-in failed:" + e.getMessage(), e);
            LOG.debug(e.getMessage(), e);

            authenticated = false;
        }

        return authenticated;
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

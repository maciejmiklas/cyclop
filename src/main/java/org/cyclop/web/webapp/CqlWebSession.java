package org.cyclop.web.webapp;

import org.apache.wicket.authroles.authentication.AuthenticatedWebSession;
import org.apache.wicket.authroles.authorization.strategies.role.Roles;
import org.apache.wicket.injection.Injector;
import org.apache.wicket.protocol.http.servlet.ServletWebRequest;
import org.apache.wicket.request.Request;
import org.apache.wicket.request.cycle.RequestCycle;
import org.cyclop.common.AppConfig;
import org.cyclop.service.cassandra.CassandraSession;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.inject.Inject;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpSession;

/** @author Maciej Miklas */
public class CqlWebSession extends AuthenticatedWebSession {
	private final static Logger LOG = LoggerFactory.getLogger(CqlWebSession.class);

	@Inject
	private CassandraSession cassandraSession;

	private AppConfig conf = AppConfig.get();

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
			bindExpirationListener();

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

	private void bindExpirationListener() {
		ServletWebRequest webRequest = (ServletWebRequest) RequestCycle.get().getRequest();
		HttpServletRequest servRequest = webRequest.getContainerRequest();
		HttpSession session = servRequest.getSession();
		session.setMaxInactiveInterval(conf.httpSession.expirySeconds);

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

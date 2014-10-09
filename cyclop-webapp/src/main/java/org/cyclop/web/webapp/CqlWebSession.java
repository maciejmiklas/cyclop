/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.cyclop.web.webapp;

import java.io.IOException;

import javax.inject.Inject;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpSession;

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

import com.datastax.driver.core.exceptions.AuthenticationException;

/** @author Maciej Miklas */
public class CqlWebSession extends AuthenticatedWebSession {
	private final static Logger LOG = LoggerFactory.getLogger(CqlWebSession.class);

	@Inject
	private transient CassandraSession cassandraSession;

	private final AppConfig conf = AppConfig.get();

	private boolean authenticated = false;

	private String lastLoginError = null;

	public CqlWebSession(Request request) {
		super(request);
		Injector.get().inject(this);
	}

	private void readObject(java.io.ObjectInputStream in) throws ClassNotFoundException, IOException {
		in.defaultReadObject();
		Injector.get().inject(this);
	}

	@Override
	public boolean authenticate(String username, String password) {
		try {
			cassandraSession.authenticate(username, password);
			bindExpirationListener();

			authenticated = true;
			lastLoginError = null;
			LOG.info("Log-in succesfull: " + username);
		} catch (AuthenticationException e) {
			lastLoginError = e.getMessage();
			authenticated = false;
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

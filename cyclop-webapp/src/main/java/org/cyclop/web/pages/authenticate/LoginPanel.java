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
package org.cyclop.web.pages.authenticate;

import static org.cyclop.common.StringHelper.toInetAddress;

import java.net.InetAddress;
import java.util.Optional;

import javax.inject.Inject;
import javax.servlet.http.HttpServletRequest;

import org.apache.wicket.authroles.authentication.panel.SignInPanel;
import org.apache.wicket.request.cycle.RequestCycle;
import org.apache.wicket.request.http.WebRequest;
import org.cyclop.service.security.BruteForceService;
import org.cyclop.web.webapp.CqlWebSession;

/** @author Maciej Miklas */
public class LoginPanel extends SignInPanel {

	@Inject
	private BruteForceService bruteForce;

	public LoginPanel(String id) {
		super(id);
	}

	@Override
	protected void onSignInFailed() {
		CqlWebSession session = (CqlWebSession) getWebSession();
		String lastLoginError = session.getLastLoginError();

		WebRequest req = (WebRequest) RequestCycle.get().getRequest();
		HttpServletRequest httpReq = (HttpServletRequest) req.getContainerRequest();

		Optional<InetAddress> clientIp = getClientIp(httpReq);
		Optional<InetAddress> proxyIp = getProxyIp(httpReq);
		bruteForce.loginFailed(lastLoginError, clientIp, proxyIp);

		if (lastLoginError != null) {
			error(lastLoginError);
		} else {
			super.onSignInFailed();
		}
	}

	private Optional<InetAddress> getProxyIp(HttpServletRequest httpReq) {
		String clientIpStr = httpReq.getRemoteAddr();
		Optional<InetAddress> addr = toInetAddress(clientIpStr);
		return addr;
	}

	private Optional<InetAddress> getClientIp(HttpServletRequest httpReq) {
		String clientIpStr = httpReq.getHeader("X-Forwarded-For");
		Optional<InetAddress> addr = toInetAddress(clientIpStr);
		return addr;
	}

}

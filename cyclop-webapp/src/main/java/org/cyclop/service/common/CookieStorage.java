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
package org.cyclop.service.common;

import java.util.List;
import java.util.Optional;

import javax.inject.Inject;
import javax.inject.Named;
import javax.servlet.http.Cookie;
import javax.validation.Valid;
import javax.validation.constraints.NotNull;

import org.apache.commons.lang.StringUtils;
import org.apache.wicket.request.cycle.RequestCycle;
import org.apache.wicket.request.http.WebRequest;
import org.apache.wicket.request.http.WebResponse;
import org.cyclop.common.AppConfig;
import org.cyclop.service.converter.JsonMarshaller;
import org.cyclop.validation.EnableValidation;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/** @author Maciej Miklas */
@Named
@EnableValidation
public class CookieStorage {

	@Inject
	private JsonMarshaller marshaller;

	@Inject
	private AppConfig appConfig;

	private final static Logger LOG = LoggerFactory.getLogger(CookieStorage.class);

	public static enum CookieName {
		cyclop_prefs, cyclop_userid;
	}

	public void storeCookieAsJson(@NotNull CookieName cookieName, @NotNull Object obj) {
		String objStr = marshaller.marshal(obj);
		storeCookie(cookieName, objStr);
	}

	public @Valid <T> Optional<T> readCookieAsJson(@NotNull CookieName cookieName, @NotNull Class<T> clazz) {
		try {
			Optional<Cookie> cookie = readCookie(cookieName);
			Optional<String> cookieValue = cookie.map(c -> StringUtils.trimToNull(c.getValue()));
			if (!cookieValue.isPresent()) {
				return Optional.empty();
			}
			T obj = marshaller.unmarshal(clazz, cookieValue.get());
			return Optional.of(obj);
		} catch (Exception e) {
			LOG.warn("Error reading cookie {}, Reason: {}", cookieName, e.getMessage());
			LOG.debug(e.getMessage(), e);
			return Optional.empty();
		}
	}

	protected void storeCookie(@NotNull CookieName name, @NotNull String value) {
		RequestCycle requestCycle = RequestCycle.get();
		if (requestCycle == null) {
			LOG.warn("RequestCycle is null - cannot read cookies");
			return;
		}
		WebResponse response = (WebResponse) requestCycle.getResponse();
		Cookie cookie = new Cookie(name.name(), value);
		cookie.setMaxAge(appConfig.cookie.expirySeconds);
		response.addCookie(cookie);
	}

	protected @NotNull Optional<Cookie> readCookie(@NotNull CookieName name) {
		try {
			RequestCycle requestCycle = RequestCycle.get();
			if (requestCycle == null) {
				LOG.warn("RequestCycle is null - cannot read cookies");
				return Optional.empty();
			}
			WebRequest request = (WebRequest) requestCycle.getRequest();
			List<Cookie> cookies = request.getCookies();
			LOG.debug("Found cookies {} for {}", cookies, request);
			if (cookies == null || cookies.isEmpty()) {
				return Optional.empty();
			}

			Optional<Cookie> result = cookies.stream().filter(cookie -> name.name().equals(cookie.getName()))
					.findFirst();
			LOG.debug("Found cookie: {}", result);
			return result;
		} catch (Exception e) {
			LOG.warn("Error reading cookie {}, Reason: {}", name, e.getMessage());
			LOG.debug(e.getMessage(), e);
			return Optional.empty();
		}
	}

}

package org.cyclop.service.common;

import org.apache.commons.lang.StringUtils;
import org.apache.wicket.request.cycle.RequestCycle;
import org.apache.wicket.request.http.WebRequest;
import org.apache.wicket.request.http.WebResponse;
import org.cyclop.common.AppConfig;
import org.cyclop.service.converter.JsonMarshaller;
import org.cyclop.validation.EnableValidation;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.inject.Inject;
import javax.inject.Named;
import javax.servlet.http.Cookie;
import javax.validation.Valid;
import javax.validation.constraints.NotNull;
import java.util.List;

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

	public
	@Valid
	<T> T readCookieAsJson(@NotNull CookieName cookieName, @NotNull Class<T> clazz) {
		try {
			Cookie cookie = readCookie(cookieName);
			if (cookie == null) {
				return null;
			}
			String cookieValue = StringUtils.trimToNull(cookie.getValue());
			if (cookieValue == null) {
				return null;
			}
			T obj = marshaller.unmarshal(clazz, cookieValue);
			return obj;
		} catch (Exception e) {
			LOG.warn("Error reading cookie {}, Reason: {}", cookieName, e.getMessage());
			LOG.debug(e.getMessage(), e);
			return null;
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

	protected
	@Valid
	Cookie readCookie(@NotNull CookieName name) {
		try {
			RequestCycle requestCycle = RequestCycle.get();
			if (requestCycle == null) {
				LOG.warn("RequestCycle is null - cannot read cookies");
				return null;
			}
			WebRequest request = (WebRequest) requestCycle.getRequest();
			List<Cookie> cookies = request.getCookies();
			LOG.debug("Found cookies {} for {}", cookies, request);
			if (cookies == null || cookies.isEmpty()) {
				return null;
			}
			for (Cookie cookie : cookies) {
				if (name.name().equals(cookie.getName())) {
					LOG.debug("Found cookie: {}", cookie);
					return cookie;
				}
			}
			return null;
		} catch (Exception e) {
			LOG.warn("Error reading cookie {}, Reason: {}", name, e.getMessage());
			LOG.debug(e.getMessage(), e);
			return null;
		}
	}

}

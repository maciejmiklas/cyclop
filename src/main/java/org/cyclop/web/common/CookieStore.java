package org.cyclop.web.common;

import java.util.List;
import javax.inject.Inject;
import javax.inject.Named;
import javax.servlet.http.Cookie;
import org.apache.commons.lang.StringUtils;
import org.apache.wicket.request.cycle.RequestCycle;
import org.apache.wicket.request.http.WebRequest;
import org.apache.wicket.request.http.WebResponse;
import org.cyclop.common.AppConfig;
import org.cyclop.service.converter.JsonMarshaller;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author Maciej Miklas
 */
@Named
public class CookieStore {

    @Inject
    private JsonMarshaller marshaller;

    @Inject
    private AppConfig appConfig;

    private final static Logger LOG = LoggerFactory.getLogger(CookieStore.class);

    public static enum CookieName {
        cyclop_prefs;
    }

    public void storeCookieAsJson(CookieName cookieName, Object obj) {
        String objStr = marshaller.marshal(obj);
        storeCookie(cookieName, objStr);
    }

    public <T> T readCookieAsJson(CookieName cookieName, Class<T> clazz) {
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
    }

    public void storeCookie(CookieName name, String value) {
        RequestCycle requestCycle = RequestCycle.get();
        WebResponse response = (WebResponse) requestCycle.getResponse();
        Cookie cookie = new Cookie(name.name(), value);
        cookie.setMaxAge(appConfig.cookie.expirySeconds);
        response.addCookie(cookie);
    }

    public Cookie readCookie(CookieName name) {
        RequestCycle requestCycle = RequestCycle.get();
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
    }

}

package org.cyclop.web.common;

import javax.inject.Inject;
import javax.inject.Named;
import org.cyclop.model.UserPreferences;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author Maciej Miklas
 */
@Named
public class UserPreferencesStore {

    private final static Logger LOG = LoggerFactory.getLogger(UserPreferencesStore.class);

    @Inject
    CookieStore cookieStore;

    public void store(UserPreferences preferences) {
        cookieStore.storeCookieAsJson(CookieStore.CookieName.cyclop_prefs,preferences);
    }

    public UserPreferences read() {
        UserPreferences preferences = cookieStore.readCookieAsJson(CookieStore.CookieName.cyclop_prefs, UserPreferences.class);
        if (preferences == null) {
            LOG.debug("User preferences not found as cookie - using default");
            preferences = new UserPreferences();
        }
        return preferences;
    }
}

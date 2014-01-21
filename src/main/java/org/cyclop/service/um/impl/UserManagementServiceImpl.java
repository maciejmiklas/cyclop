package org.cyclop.service.um.impl;

import javax.inject.Inject;
import javax.inject.Named;
import org.cyclop.model.UserIdentifier;
import org.cyclop.model.UserPreferences;
import org.cyclop.service.cookie.CookieStorage;
import org.cyclop.service.um.UserManagementService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author Maciej Miklas
 */
@Named
public class UserManagementServiceImpl implements UserManagementService {

    private final static Logger LOG = LoggerFactory.getLogger(UserManagementServiceImpl.class);

    @Inject
    private CookieStorage cookieStorage;

    public void storePreferences(UserPreferences preferences) {
        cookieStorage.storeCookieAsJson(CookieStorage.CookieName.cyclop_prefs,preferences);
    }

    public UserPreferences readPreferences() {
        UserPreferences preferences = cookieStorage.readCookieAsJson(CookieStorage.CookieName.cyclop_prefs, UserPreferences.class);
        if (preferences == null) {
            LOG.debug("User preferences not found as cookie - using default");
            preferences = new UserPreferences();
        }
        return preferences;
    }



    @Override
    public void storeIdentifier(UserIdentifier id) {
        cookieStorage.storeCookieAsJson(CookieStorage.CookieName.cyclop_userid,id);
    }

    @Override
    public UserIdentifier readIdentifier() {
        UserIdentifier id = cookieStorage.readCookieAsJson(CookieStorage.CookieName.cyclop_userid, UserIdentifier.class);
        if (id == null) {
            LOG.debug("User Identifier not found as cookie - generating new");
            id = new UserIdentifier();
        }

        return id;
    }
}

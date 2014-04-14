package org.cyclop.service.um.impl;

import org.cyclop.model.UserIdentifier;
import org.cyclop.model.UserPreferences;
import org.cyclop.service.common.CookieStorage;
import org.cyclop.service.um.UserManager;
import org.cyclop.validation.EnableValidation;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.inject.Inject;
import javax.inject.Named;

/** @author Maciej Miklas */
@Named
@EnableValidation
public class CookieBasedUserManager implements UserManager {

    private final static Logger LOG = LoggerFactory.getLogger(CookieBasedUserManager.class);

    @Inject
    private CookieStorage cookieStorage;

    @Override
    public boolean storePreferences(UserPreferences preferences) {
	UserPreferences readPrefs = readPreferences();
	if (readPrefs.equals(preferences)) {
	    LOG.debug("Preferences did not change - no update required {}", preferences);
	    return false;
	}
	LOG.debug("Updating preferences {}", preferences);
	cookieStorage.storeCookieAsJson(CookieStorage.CookieName.cyclop_prefs, preferences);
	return true;
    }

    @Override
    public UserPreferences readPreferences() {
	UserPreferences preferences = cookieStorage.readCookieAsJson(
		CookieStorage.CookieName.cyclop_prefs,
		UserPreferences.class);
	if (preferences == null) {
	    LOG.debug("User preferences not found as cookie - using default");
	    preferences = new UserPreferences();
	}
	return preferences;
    }

    @Override
    public void storeIdentifier(UserIdentifier id) {
	cookieStorage.storeCookieAsJson(CookieStorage.CookieName.cyclop_userid, new UserIdentifierCookie(id));
    }

    @Override
    public UserIdentifier readIdentifier() {
	UserIdentifier id = null;
	UserIdentifierCookie cookie = cookieStorage.readCookieAsJson(
		CookieStorage.CookieName.cyclop_userid,
		UserIdentifierCookie.class);
	if (cookie == null) {
	    id = new UserIdentifier();
	    storeIdentifier(id);
	    LOG.info("Generated new User Identifier: " + id.id);
	}
	else {
	    id = cookie.getId();
	}

	return id;
    }
}

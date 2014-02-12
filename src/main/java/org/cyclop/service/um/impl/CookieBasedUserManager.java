package org.cyclop.service.um.impl;

import org.cyclop.model.UserIdentifier;
import org.cyclop.model.UserPreferences;
import org.cyclop.service.common.CookieStorage;
import org.cyclop.service.um.UserManager;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.inject.Inject;
import javax.inject.Named;

/** @author Maciej Miklas */
@Named
public class CookieBasedUserManager implements UserManager {

	private final static Logger LOG = LoggerFactory.getLogger(CookieBasedUserManager.class);

	@Inject
	private CookieStorage cookieStorage;

	public void storePreferences(UserPreferences preferences) {
		cookieStorage.storeCookieAsJson(CookieStorage.CookieName.cyclop_prefs, preferences);
	}

	public UserPreferences readPreferences() {
		UserPreferences preferences = cookieStorage
				.readCookieAsJson(CookieStorage.CookieName.cyclop_prefs, UserPreferences.class);
		if (preferences == null) {
			LOG.debug("User preferences not found as cookie - using default");
			preferences = new UserPreferences();
		}
		return preferences;
	}

	@Override
	public void registerIdentifier(UserIdentifier id) {
		cookieStorage.storeCookieAsJson(CookieStorage.CookieName.cyclop_userid, id);
	}

	@Override
	public UserIdentifier readIdentifier() {
		UserIdentifier id = cookieStorage.readCookieAsJson(CookieStorage.CookieName.cyclop_userid, UserIdentifier.class);
		if (id == null) {
			LOG.debug("User Identifier not found as cookie - generating new");
			id = new UserIdentifier();
			cookieStorage.storeCookieAsJson(CookieStorage.CookieName.cyclop_userid, id);
		}

		return id;
	}
}

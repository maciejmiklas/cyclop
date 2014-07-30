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
package org.cyclop.service.um.impl;

import java.util.Optional;

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
		Optional<UserPreferences> preferencesOpt = cookieStorage.readCookieAsJson(
				CookieStorage.CookieName.cyclop_prefs, UserPreferences.class);
		UserPreferences preferences = preferencesOpt.orElseGet(UserPreferences::new);
		// TODO move log statement to optional above
		if (!preferencesOpt.isPresent()) {
			LOG.debug("User preferences not found as cookie - using default");
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
		Optional<UserIdentifierCookie> cookie = cookieStorage.readCookieAsJson(CookieStorage.CookieName.cyclop_userid,
				UserIdentifierCookie.class);
		if (!cookie.isPresent()) {
			id = new UserIdentifier();
			storeIdentifier(id);
			LOG.info("Generated new User Identifier: " + id.id);
		} else {
			id = cookie.get().getId();
		}

		return id;
	}
}

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
package org.cyclop.service.queryprotocoling.impl;

import java.util.Optional;
import java.util.concurrent.atomic.AtomicReference;

import javax.inject.Inject;
import javax.inject.Named;
import javax.validation.constraints.NotNull;

import net.jcip.annotations.NotThreadSafe;

import org.cyclop.model.UserIdentifier;
import org.cyclop.service.common.FileStorage;
import org.cyclop.service.queryprotocoling.QueryProtocolingService;
import org.cyclop.service.um.UserManager;
import org.cyclop.validation.EnableValidation;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.annotation.Scope;
import org.springframework.context.annotation.ScopedProxyMode;

/** @author Maciej Miklas */
@NotThreadSafe
@Named
@EnableValidation
@Scope(value = "session", proxyMode = ScopedProxyMode.TARGET_CLASS)
abstract class AbstractQueryProtocolingService<H> implements QueryProtocolingService<H> {

	private final static Logger LOG = LoggerFactory.getLogger(AbstractQueryProtocolingService.class);

	private final AtomicReference<H> history = new AtomicReference<>();

	@Inject
	private UserManager um;

	@Inject
	private FileStorage storage;

	@Inject
	private AsyncFileStore<H> asyncFileStore;

	private UserIdentifier identifier;

	protected abstract Class<H> getClazz();

	protected abstract H createEmpty();

	protected UserIdentifier getUser() {
		UserIdentifier fromCookie = um.readIdentifier();
		if (identifier == null) {
			LOG.debug("Using identifier from cookie: {}", fromCookie);
			identifier = fromCookie;
		} else {

			// make sure that there is only one user identifier pro http session
			// if it's not the case overwrite new one with existing one
			if (!fromCookie.equals(identifier)) {
				LOG.debug("Replacing {} with {}", fromCookie, identifier);
				um.storeIdentifier(identifier);
			}
		}
		return identifier;
	}

	@Override
	public void store(@NotNull H newHistory) {
		LOG.debug("String history");
		history.set(newHistory);
		UserIdentifier user = getUser();

		// this synchronization ensures that history will be not added to write
		// queue while it's being read from
		// disk in #readHistory()
		synchronized (asyncFileStore) {
			asyncFileStore.store(user, newHistory);
		}
	}

	@Override
	public @NotNull H read() {
		LOG.debug("Accessing history");
		if (history.get() == null) {
			synchronized (asyncFileStore) {
				LOG.debug("Reading history");
				if (history.get() == null) {
					UserIdentifier user = getUser();

					// history can be in write queue when user closes http
					// session and opens new few seconds later.
					// in this case async queue might be not flushed to disk yet
					Optional<H> readOpt = asyncFileStore.getFromWriteQueue(user);
					H read = null;
					if (!readOpt.isPresent()) {
						read = storage.read(user, getClazz()).orElse(createEmpty());
					}
					history.set(read);
				}
			}
		}
		return history.get();
	}

	@Override
	public boolean supported() {
		return storage.supported();
	}
}

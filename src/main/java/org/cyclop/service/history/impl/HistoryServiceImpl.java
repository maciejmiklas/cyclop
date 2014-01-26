package org.cyclop.service.history.impl;

import org.cyclop.model.QueryHistory;
import org.cyclop.model.UserIdentifier;
import org.cyclop.service.common.FileStorage;
import org.cyclop.service.history.HistoryService;
import org.cyclop.service.um.UserManager;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.annotation.Scope;
import org.springframework.context.annotation.ScopedProxyMode;

import javax.annotation.concurrent.NotThreadSafe;
import javax.inject.Inject;
import javax.inject.Named;
import java.util.concurrent.atomic.AtomicReference;

/**
 * @author Maciej Miklas
 */
@NotThreadSafe
@Named
@Scope(value = "session", proxyMode = ScopedProxyMode.TARGET_CLASS)
class HistoryServiceImpl implements HistoryService {

    private final static Logger LOG = LoggerFactory.getLogger(HistoryServiceImpl.class);

    @Inject
    private UserManager um;

    @Inject
    private FileStorage storage;

    @Inject
    private AsyncFileStore asyncFileStore;

    private UserIdentifier identifier;

    private final AtomicReference<QueryHistory> history = new AtomicReference<>();

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
                um.registerIdentifier(identifier);
            }
        }

        return identifier;
    }

    @Override
    public void store(QueryHistory newHistory) {
        if (newHistory == null) {
            throw new IllegalArgumentException("History cannot be null");
        }

        history.set(newHistory);
        UserIdentifier user = getUser();

        // this synchronization ensures that history will be not added to write queue while it's being read from
        // disk in #readHistory()
        synchronized (asyncFileStore) {
            asyncFileStore.store(user, newHistory);
        }
    }

    @Override
    public QueryHistory readHistory() {
        if (history.get() == null) {
            synchronized (asyncFileStore) {
                if (history.get() == null) {
                    UserIdentifier user = getUser();

                    // history can be in write queue when user closes http session and opens new few seconds later.
                    // in this case async queue might be not flushed to disk yet
                    QueryHistory read = asyncFileStore.getFromWriteQueue(user);
                    if (read == null) {
                        read = storage.readHistory(user);
                    }
                    if (read == null) {
                        history.set(new QueryHistory());
                    }
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

package org.cyclop.service.history.impl;

import java.util.concurrent.atomic.AtomicReference;
import java.util.concurrent.locks.ReadWriteLock;
import java.util.concurrent.locks.ReentrantReadWriteLock;

import javax.annotation.concurrent.NotThreadSafe;
import javax.inject.Inject;
import javax.inject.Named;

import org.cyclop.model.QueryHistory;
import org.cyclop.model.UserIdentifier;
import org.cyclop.service.history.HistoryService;
import org.cyclop.service.um.UserManagementService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.annotation.Scope;
import org.springframework.context.annotation.ScopedProxyMode;

/**
 * @author Maciej Miklas
 */
@NotThreadSafe
@Named
@Scope(value = "session", proxyMode = ScopedProxyMode.TARGET_CLASS)
public class HistoryServiceImpl implements HistoryService {

    private final ReadWriteLock historyLock = new ReentrantReadWriteLock();

    private final static Logger LOG = LoggerFactory.getLogger(HistoryServiceImpl.class);

    @Inject
    private UserManagementService um;

    @Inject
    private FileStorage storage;

    private UserIdentifier identifier;

    private final AtomicReference<QueryHistory> history = new AtomicReference<>();

    private UserIdentifier getUser() {
        UserIdentifier cookieIdentifier = um.readIdentifier();
        if (identifier == null) {
            identifier = cookieIdentifier;
        } else {

            // make sure that there is only one user identifier pro http session
            // if it's not the case overwrite new one with existing one
            if (!cookieIdentifier.equals(identifier)) {
                LOG.debug("Replacing {} with {}", cookieIdentifier, identifier);
                um.storeIdentifier(identifier);
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

        // TODO add to queue
    }

    @Override
    public QueryHistory readHistory() {
        if (history.get() == null) {
            synchronized (this) {
                if (history.get() == null) {
                    UserIdentifier user = getUser();
                    QueryHistory read = storage.readHistory(user);
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

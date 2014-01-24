package org.cyclop.service.history.impl;

import org.cyclop.model.QueryHistory;
import org.cyclop.model.UserIdentifier;

import javax.inject.Named;
import java.util.HashMap;
import java.util.Map;

/**
 * @author Maciej Miklas
 */
@Named
class AsyncFileStore {

    private final Map<UserIdentifier,QueryHistory> historyMap = new HashMap<>();

    public synchronized void store(UserIdentifier identifier, QueryHistory history) {

    }

    public synchronized QueryHistory getFromWriteQueue(UserIdentifier identifier) {
        return null;
    }

}

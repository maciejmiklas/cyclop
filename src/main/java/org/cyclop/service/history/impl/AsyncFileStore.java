package org.cyclop.service.history.impl;

import org.cyclop.model.QueryHistory;
import org.cyclop.model.UserIdentifier;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.inject.Inject;
import javax.inject.Named;
import java.util.HashMap;
import java.util.Map;

/**
 * @author Maciej Miklas
 */
@Named
class AsyncFileStore {

    @Inject
    private FileStorage fileStorage;

    private final static Logger LOG = LoggerFactory.getLogger(AsyncFileStore.class);

    private final Map<UserIdentifier, QueryHistory> historyMap = new HashMap<>();

    public void store(UserIdentifier identifier, QueryHistory history) {
        synchronized (historyMap) {
            historyMap.put(identifier, history);
        }
    }

    public QueryHistory getFromWriteQueue(UserIdentifier identifier) {
        synchronized (historyMap) {
            return historyMap.get(identifier);
        }
    }

    public synchronized void flush() {
        while (true) {
            UserIdentifier identifier = null;
            QueryHistory history = null;

            // synchronize #historyMap only for short time to not block store(...) function by file operation
            synchronized (historyMap) {
                if (historyMap.isEmpty()) {
                    LOG.debug("Flush done - no more entries found");
                    return;
                }
                identifier = historyMap.keySet().iterator().next();
                history = historyMap.remove(identifier);
            }
            fileStorage.storeHistory(identifier, history);
        }
    }


}

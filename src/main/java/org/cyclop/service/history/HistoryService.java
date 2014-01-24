package org.cyclop.service.history;

import org.cyclop.model.QueryHistory;

/**
 * HTTP session scoped history service.
 * 
 * @author Maciej Miklas
 */
public interface HistoryService {

    void store(QueryHistory history);

    QueryHistory readHistory();

    boolean supported();
}

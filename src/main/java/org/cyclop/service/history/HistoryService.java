package org.cyclop.service.history;

import org.cyclop.model.QueryHistory;

/**
 * HTTP session scoped history service.
 *
 * @author Maciej Miklas
 */
public interface HistoryService {

	void store(QueryHistory history);

	/** @return never null, creates empty if not found on disk */
	QueryHistory readHistory();

	boolean supported();
}

package org.cyclop.service.queryprotocoling;

import org.cyclop.model.QueryEntry;
import org.cyclop.model.QueryHistory;

import javax.validation.constraints.NotNull;

/**
 * HTTP session scoped history service.
 *
 * @author Maciej Miklas
 */
public interface HistoryService extends QueryProtocolingService<QueryHistory> {

	// TODO validation test
	void addAndStore(@NotNull QueryEntry entry);
}

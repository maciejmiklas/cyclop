package org.cyclop.service.queryprotocoling.impl;

import net.jcip.annotations.NotThreadSafe;
import org.cyclop.model.QueryEntry;
import org.cyclop.model.QueryHistory;
import org.cyclop.service.queryprotocoling.HistoryService;
import org.springframework.context.annotation.Scope;
import org.springframework.context.annotation.ScopedProxyMode;
import org.springframework.validation.annotation.Validated;

import javax.inject.Named;
import javax.validation.constraints.NotNull;

/** @author Maciej Miklas */
@NotThreadSafe
@Named
@Scope(value = "session", proxyMode = ScopedProxyMode.TARGET_CLASS)
@Validated
public class HistoryServiceImpl extends AbstractQueryProtocolingService<QueryHistory> implements HistoryService {

	protected HistoryServiceImpl() {
	}

	@Override
	protected Class<QueryHistory> getClazz() {
		return QueryHistory.class;
	}

	@Override
	protected QueryHistory createEmpty() {
		return new QueryHistory();
	}

	@Override
	public void addAndStore(@NotNull QueryEntry entry) {
		QueryHistory hist = read();
		hist.add(entry);
		store(hist);
	}
}

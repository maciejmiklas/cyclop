package org.cyclop.service.queryprotocoling.impl;

import net.jcip.annotations.NotThreadSafe;
import org.cyclop.model.QueryHistory;
import org.cyclop.service.queryprotocoling.HistoryService;
import org.springframework.context.annotation.Scope;
import org.springframework.context.annotation.ScopedProxyMode;

import javax.inject.Named;

/** @author Maciej Miklas */
@NotThreadSafe
@Named
@Scope(value = "session", proxyMode = ScopedProxyMode.TARGET_CLASS)
class HistoryServiceImpl extends AbstractQueryProtocolingService<QueryHistory> implements HistoryService {

	@Override
	protected Class<QueryHistory> getClazz() {
		return QueryHistory.class;
	}

	@Override
	protected QueryHistory createEmpty() {
		return new QueryHistory();
	}

}

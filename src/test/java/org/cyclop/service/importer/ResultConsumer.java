package org.cyclop.service.importer;

import net.jcip.annotations.ThreadSafe;
import org.cyclop.model.CqlQuery;
import org.cyclop.model.exception.QueryException;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

/** @author Maciej Miklas */
@ThreadSafe
class ResultConsumer implements ResultWriter {

	public List<CqlQuery> success = new ArrayList<>();

	public Map<CqlQuery, Exception> error = new HashMap<>();

	@Override
	public synchronized void success(CqlQuery query, long runtime) {
		assertTrue(runtime >= 0);
		success.add(query);
	}

	@Override
	public synchronized void error(CqlQuery query, QueryException ex, long runtime) {
		assertTrue(runtime >= 0);
		error.put(query, ex);
	}

	@Override
	public synchronized void unknownError(CqlQuery query, Exception error, long runtime) {
		fail();
	}

	public synchronized int size() {
		return success.size() + error.size();
	}

	@Override
	public synchronized String toString() {
		return "ResultConsumer [success=" + success + ", error=" + error + "]";
	}


}

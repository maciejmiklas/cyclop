package org.cyclop.service.importer;

import org.cyclop.model.CqlQuery;
import org.cyclop.model.exception.QueryException;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

/** @author Maciej Miklas */
class ResultConsumer implements ResultWriter {

	public List<CqlQuery> success = new ArrayList<>();

	public Map<CqlQuery, Exception> error = new HashMap<>();

	@Override
	public void success(CqlQuery query, long runtime) {
		assertTrue(runtime >= 0);
		success.add(query);
	}

	@Override
	public void error(CqlQuery query, QueryException ex, long runtime) {
		assertTrue(runtime >= 0);
		error.put(query, ex);
	}

	@Override
	public void unknownError(CqlQuery query, Exception error, long runtime) {
		fail();
	}

	public int size() {
		return success.size() + error.size();
	}

	@Override
	public String toString() {
		return "ResultConsumer [success=" + success + ", error=" + error + "]";
	}


}

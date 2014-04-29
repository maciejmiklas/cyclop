package org.cyclop.web.panels.queryimport;

import org.cyclop.model.CqlQuery;

/** @author Maciej Miklas */
class ImportResult {
	public final CqlQuery query;

	public final String error;

	public final long runTime;

	public ImportResult(CqlQuery query, long runtime) {
		this(query, null, runtime);
	}

	public ImportResult(CqlQuery query, String error, long runtime) {
		this.query = query;
		this.error = error;
		this.runTime = runtime;
	}

	@Override
	public String toString() {
		return "ImportResult [query=" + query + ", error=" + error + ", runtime=" + runTime + "]";
	}
}

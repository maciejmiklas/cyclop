package org.cyclop.web.panels.queryimport;

import org.cyclop.model.CqlQuery;
import org.cyclop.model.exception.QueryException;

/** @author Maciej Miklas */
class ImportResult {
    public final CqlQuery query;
    public final QueryException error;
    public final long runTime;

    public ImportResult(CqlQuery query, long runtime) {
	this(query, null, runtime);
    }

    public ImportResult(CqlQuery query, QueryException error, long runtime) {
	this.query = query;
	this.error = error;
	this.runTime = runtime;
    }

    @Override
    public String toString() {
	return "ImportResult [query=" + query + ", error=" + error + ", runtime=" + runTime + "]";
    }
}

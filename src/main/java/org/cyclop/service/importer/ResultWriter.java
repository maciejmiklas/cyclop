package org.cyclop.service.importer;

import org.cyclop.model.CqlQuery;
import org.cyclop.model.exception.QueryException;

/**
 * IMPLEMENTATION MUST BE THREAD-SAVE
 *
 * @author Maciej Miklas
 */
public interface ResultWriter {

	void success(CqlQuery query, long runtime);

	void error(CqlQuery query, QueryException error, long runtime);

	void unknownError(CqlQuery query, Exception error, long runtime);
}

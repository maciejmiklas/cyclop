package org.cyclop.service.importer;

import net.jcip.annotations.ThreadSafe;
import org.cyclop.model.CqlQuery;
import org.cyclop.model.exception.QueryException;

/**
 * IMPLEMENTATION MUST BE THREAD-SAVE
 *
 * @author Maciej Miklas
 */
@ThreadSafe
public interface ResultWriter {

	void success(CqlQuery query, long runtime);

	void error(CqlQuery query, QueryException error, long runtime);

	void unknownError(CqlQuery query, Exception error, long runtime);
}

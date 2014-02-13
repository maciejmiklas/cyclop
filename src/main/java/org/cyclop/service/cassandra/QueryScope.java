package org.cyclop.service.cassandra;

import org.cyclop.model.CqlKeySpace;

/**
 * Session bound query scope
 *
 * @author Maciej Miklas
 */
public interface QueryScope {

	CqlKeySpace getActiveKeySpace();
}

package org.cyclop.service.cassandra;

import com.google.common.collect.ImmutableSortedSet;
import org.cyclop.model.CqlColumnName;
import org.cyclop.model.CqlIndex;
import org.cyclop.model.CqlKeySpace;
import org.cyclop.model.CqlQuery;
import org.cyclop.model.CqlSelectResult;
import org.cyclop.model.CqlTable;

/** @author Maciej Miklas */
public interface QueryService {

	ImmutableSortedSet<CqlColumnName> findColumnNames(CqlTable table);

	boolean checkTableExists(CqlTable table);

	ImmutableSortedSet<CqlColumnName> findAllColumnNames();

	ImmutableSortedSet<CqlIndex> findAllIndexes(CqlKeySpace keySpace);

	ImmutableSortedSet<CqlKeySpace> findAllKeySpaces();

	CqlSelectResult execute(CqlQuery query);

	ImmutableSortedSet<CqlTable> findTableNames(CqlKeySpace keySpace);
}

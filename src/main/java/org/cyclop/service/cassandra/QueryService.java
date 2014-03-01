package org.cyclop.service.cassandra;

import com.google.common.collect.ImmutableSortedSet;
import org.cyclop.model.CqlColumnName;
import org.cyclop.model.CqlIndex;
import org.cyclop.model.CqlKeySpace;
import org.cyclop.model.CqlQuery;
import org.cyclop.model.CqlSelectResult;
import org.cyclop.model.CqlTable;

import javax.validation.constraints.NotNull;

/** @author Maciej Miklas */
public interface QueryService {

	@NotNull
	ImmutableSortedSet<CqlColumnName> findColumnNames(@NotNull CqlTable table);

	boolean checkTableExists(CqlTable table);

	@NotNull
	ImmutableSortedSet<CqlColumnName> findAllColumnNames();

	@NotNull
	ImmutableSortedSet<CqlIndex> findAllIndexes(@NotNull CqlKeySpace keySpace);

	@NotNull
	ImmutableSortedSet<CqlKeySpace> findAllKeySpaces();

	@NotNull
	CqlSelectResult execute(@NotNull CqlQuery query);

	@NotNull
	ImmutableSortedSet<CqlTable> findTableNames(@NotNull CqlKeySpace keySpace);
}

package org.cyclop.service.cassandra;

import com.google.common.collect.ImmutableSortedSet;
import org.cyclop.model.CqlColumnName;
import org.cyclop.model.CqlIndex;
import org.cyclop.model.CqlKeySpace;
import org.cyclop.model.CqlQuery;
import org.cyclop.model.CqlQueryResult;
import org.cyclop.model.CqlTable;

import javax.validation.Valid;
import javax.validation.constraints.NotNull;

/** @author Maciej Miklas */
public interface QueryService {

	@NotNull
	ImmutableSortedSet<CqlColumnName> findColumnNames(@Valid CqlTable table);

	boolean checkTableExists(CqlTable table);

	@NotNull
	ImmutableSortedSet<CqlColumnName> findAllColumnNames();

	@NotNull
	ImmutableSortedSet<CqlIndex> findAllIndexes(@Valid CqlKeySpace keySpace);

	@NotNull
	ImmutableSortedSet<CqlKeySpace> findAllKeySpaces();

	@NotNull
	CqlQueryResult execute(@NotNull CqlQuery query);

	@NotNull
	CqlQueryResult execute(@NotNull CqlQuery query, boolean updateHistory);

	@NotNull
	ImmutableSortedSet<CqlTable> findTableNames(@Valid CqlKeySpace keySpace);
}

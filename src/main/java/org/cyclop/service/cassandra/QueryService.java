package org.cyclop.service.cassandra;

import com.google.common.collect.ImmutableSortedSet;
import org.cyclop.service.model.CqlColumnName;
import org.cyclop.service.model.CqlKeySpace;
import org.cyclop.service.model.CqlQuery;
import org.cyclop.service.model.CqlSelectResult;
import org.cyclop.service.model.CqlTable;

/**
 * @author Maciej Miklas
 */
public interface QueryService {

    ImmutableSortedSet<CqlColumnName> findColumnNames(CqlTable table);

    ImmutableSortedSet<CqlTable> findTableNamesForActiveKeySpace();

    boolean checkTableExists(CqlTable table);

    boolean checkKeyspaceExists(CqlKeySpace keySpace);

    ImmutableSortedSet<CqlColumnName> findAllColumnNames();

    ImmutableSortedSet<CqlKeySpace> findAllKeySpaces();

    CqlSelectResult execute(CqlQuery query);

    ImmutableSortedSet<CqlTable> findTableNames(CqlKeySpace keySpace);
}


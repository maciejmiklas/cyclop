package org.cyclop.service.cassandra;

import com.google.common.collect.ImmutableSortedSet;
import org.cyclop.model.*;

/**
 * @author Maciej Miklas
 */
public interface QueryService {

    ImmutableSortedSet<CqlColumnName> findColumnNames(CqlTable table);

    ImmutableSortedSet<CqlTable> findTableNamesForActiveKeySpace();

    boolean checkTableExists(CqlTable table);

    boolean checkKeyspaceExists(CqlKeySpace keySpace);

    ImmutableSortedSet<CqlColumnName> findAllColumnNames();

    ImmutableSortedSet<CqlIndex> findAllIndexes();

    ImmutableSortedSet<CqlKeySpace> findAllKeySpaces();

    CqlSelectResult execute(CqlQuery query);

    ImmutableSortedSet<CqlTable> findTableNames(CqlKeySpace keySpace);
}

package org.cyclop.service.model;

/**
 * @author Maciej Miklas
 */
public enum CqlQueryType {
    UNKNOWN, CREATE_KEYSPACE, USE, ALTER_KEYSPACE, DROP_KEYSPACE, CREATE_TABLE, ALTER_TABLE, DROP_TABLE, TRUNCATE,
    CREATE_INDEX, DROP_INDEX,
    INSERT, UPDATE, DELETE, BATCH, SELECT

}

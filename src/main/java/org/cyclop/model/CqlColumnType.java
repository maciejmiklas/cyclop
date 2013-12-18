package org.cyclop.model;

/**
 * @author Maciej Miklas
 */
public enum CqlColumnType {
    UNKNOWN, PARTITION_KEY, REGULAR, COMPACT_VALUE, CLUSTERING_KEY,
    /**
     * trick used to separate common columns from custom columns in results list
     */
    SEPARATOR
}

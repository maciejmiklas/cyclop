package org.cyclop.model;

import com.datastax.driver.core.DataType;

/**
 * @author Maciej Miklas
 */
public final class CqlPartitionKey extends CqlExtendedColumnName {

    public CqlPartitionKey(CqlColumnType columnType, DataType dataType, String columnName) {
        super(columnType, dataType, columnName);
    }

    public static CqlPartitionKey fromColumn(CqlExtendedColumnName col) {
        return new CqlPartitionKey(col.columnType, col.dataType, col.part);
    }

    @Override
    public String toString() {
        return com.google.common.base.Objects.toStringHelper(this).add("columnType", columnType).add("part",
                part).add("dataType", dataType).toString();
    }
}

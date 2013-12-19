package org.cyclop.model;

import com.datastax.driver.core.DataType;

import java.util.Objects;

/**
 * Cql keyword
 *
 * @author Maciej Miklas
 */
public class CqlExtendedColumnName extends CqlColumnName {

    public final CqlColumnType columnType;

    public CqlExtendedColumnName(CqlColumnType columnType, DataType dataType, String columnName) {
        super(dataType, columnName);
        this.columnType = columnType;
    }

    @Override
    public String toString() {
        return com.google.common.base.Objects.toStringHelper(this).add("columnType", columnType).add("part",
                part).add("dataType", dataType).toString();
    }

    @Override
    public CqlType type() {
        return CqlType.COLUMN;
    }

    @Override
    public int hashCode() {
        return Objects.hash(partLc, columnType, dataType);
    }

    @Override
    public boolean equals(Object obj) {
        if (obj == null || getClass() != obj.getClass()) {
            return false;
        }
        final CqlExtendedColumnName other = (CqlExtendedColumnName) obj;
        return Objects.equals(partLc, other.partLc) && Objects.equals(columnType, other.columnType) && Objects.equals
                (dataType, other.dataType);
    }

}

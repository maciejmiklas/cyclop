package org.cyclop.model;

import com.datastax.driver.core.DataType;

import java.util.Objects;

/**
 * @author Maciej Miklas
 */
public class CqlColumnName extends CqlPart {

    public final DataType dataType;

    public CqlColumnName(DataType dataType, String columnName) {
        super(columnName);
        this.dataType = dataType;
    }

    @Override
    public String toString() {
        return com.google.common.base.Objects.toStringHelper(this).
                add("part", part).add("dataType", dataType).toString();
    }

    @Override
    public CqlType type() {
        return CqlType.COLUMN;
    }

    @Override
    public int hashCode() {
        return Objects.hash(partLc, dataType);
    }

    @Override
    public boolean equals(Object obj) {
        if (obj == null || getClass() != obj.getClass()) {
            return false;
        }
        final CqlExtendedColumnName other = (CqlExtendedColumnName) obj;
        return Objects.equals(partLc, other.partLc) && Objects.equals(dataType, other.dataType);
    }

}

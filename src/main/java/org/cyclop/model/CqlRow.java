package org.cyclop.model;

import com.datastax.driver.core.Row;
import com.google.common.base.Objects;
import com.google.common.collect.ImmutableList;

/**
 * @author Maciej Miklas
 */
public final class CqlRow {

    public final Row original;

    public final ImmutableList<CqlExtendedColumnName> allColumns;

    public CqlRow(Row original, ImmutableList<CqlExtendedColumnName> allColumns) {
        this.original = original;
        this.allColumns = allColumns;
    }

    @Override
    public String toString() {
        return Objects.toStringHelper(this).add("original", original).add("allColumns", allColumns).toString();
    }
}

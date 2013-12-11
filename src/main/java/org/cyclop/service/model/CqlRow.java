package org.cyclop.service.model;

import com.datastax.driver.core.Row;
import com.google.common.base.Objects;
import com.google.common.collect.ImmutableList;

/**
 * @author Maciej Miklas
 */
public class CqlRow {

    public final Row orginal;

    public final ImmutableList<CqlExtendedColumnName> allColumns;

    public CqlRow(Row orginal, ImmutableList<CqlExtendedColumnName> allColumns) {
        this.orginal = orginal;
        this.allColumns = allColumns;
    }

    @Override
    public String toString() {
        return Objects.toStringHelper(this).add("orginal", orginal).add("allColumns", allColumns).toString();
    }
}

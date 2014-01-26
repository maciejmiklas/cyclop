package org.cyclop.model;

import com.google.common.base.Objects;
import com.google.common.collect.ImmutableList;

import java.io.Serializable;
import javax.annotation.concurrent.Immutable;

/**
 * @author Maciej Miklas
 */
@Immutable
public final class CqlSelectResult {

    /**
     * List of columns that can be found in every row returned by the query
     */
    public final ImmutableList<CqlExtendedColumnName> commonColumns;

    /**
     * List of columns that can be found is particular rows
     */
    public final ImmutableList<CqlExtendedColumnName> dynamicColumns;

    public final ImmutableList<CqlRow> rows;

    /**
     * could be null, it not found in result, or in case of error while reading meta data info
     */
    public final CqlPartitionKey partitionKey;

    public CqlSelectResult() {
        this.commonColumns = ImmutableList.of();
        this.dynamicColumns = ImmutableList.of();
        this.rows = ImmutableList.of();
        this.partitionKey = null;
    }

    public CqlSelectResult(ImmutableList<CqlExtendedColumnName> commonColumns, ImmutableList<CqlExtendedColumnName>
            dynamicColumns, ImmutableList<CqlRow> rows, CqlPartitionKey partitionKey) {
        this.commonColumns = commonColumns;
        this.dynamicColumns = dynamicColumns;
        this.rows = rows;
        this.partitionKey = partitionKey;
    }

    public boolean isEmpty() {
        return rows == null || rows.isEmpty();
    }

    @Override
    public String toString() {
        return Objects.toStringHelper(this).add("commonColumns", commonColumns).add("dynamicColumns",
                dynamicColumns).add("rows", rows).add("partitionKey", partitionKey).toString();
    }
}

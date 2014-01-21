package org.cyclop.model;

import com.google.common.base.Objects;
import java.util.Date;
import javax.annotation.concurrent.Immutable;

/**
 * @author Maciej Miklas
 */
@Immutable
public final class QueryHistoryEntry implements Comparable<QueryHistoryEntry> {

    public final CqlQuery query;

    public final Date executedOn;

    public QueryHistoryEntry(CqlQuery query, Date executedOn) {
        this.query = query;
        this.executedOn = executedOn;
    }

    @Override
    public String toString() {
        return Objects.toStringHelper(this).add("query", query).add("executedOn", executedOn).toString();
    }

    @Override
    public int hashCode() {
        return java.util.Objects.hash(query);
    }

    @Override
    public boolean equals(final Object obj) {
        if (obj == null || getClass() != obj.getClass()) {
            return false;
        }

        final QueryHistoryEntry other = (QueryHistoryEntry) obj;
        return java.util.Objects.equals(query, other.query);
    }

    @Override
    public int compareTo(QueryHistoryEntry o) {
        return executedOn.compareTo(o.executedOn);
    }
}

package org.cyclop.model;

import com.google.common.base.Objects;
import org.joda.time.DateTime;
import org.joda.time.DateTimeZone;

import javax.annotation.concurrent.Immutable;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlTransient;
import javax.xml.bind.annotation.adapters.XmlAdapter;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

/**
 * Unique by query, sorted by date
 *
 * @author Maciej Miklas
 */
@Immutable
@XmlJavaTypeAdapter(QueryHistoryEntry.Adapter.class)
public final class QueryHistoryEntry implements Comparable<QueryHistoryEntry> {

    public final CqlQuery query;

    public final DateTime executedOnUtc;

    public QueryHistoryEntry(CqlQuery query) {
        this(query, new DateTime().toDateTime(DateTimeZone.UTC));
    }

    public QueryHistoryEntry(CqlQuery query, DateTime executedOnUtc) {
        this.query = query;
        this.executedOnUtc = executedOnUtc;
    }

    @Override
    public String toString() {
        return Objects.toStringHelper(this).add("query", query).add("executedOnUtc", executedOnUtc).toString();
    }

    @Override
    public int hashCode() {
        return java.util.Objects.hash(query);
    }

    /**
     * equals and hashCode are only on query to recognize the same queries
     */
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
        int compRes = o.executedOnUtc.compareTo(executedOnUtc);

        // just make sure that entries with the same date are not removed if query is different
        if (compRes == 0) {
            compRes = query.compareTo(o.query);
        }
        return compRes;
    }

    @XmlRootElement
    @XmlAccessorType(XmlAccessType.FIELD)
    public final static class QueryHistoryEntryJaxb {
        private CqlQuery query;
        private DateTime executedOn;

        @Override
        public String toString() {
            return Objects.toStringHelper(this).add("query", query).add("executedOn", executedOn).toString();
        }
    }

    @XmlTransient
    public final static class Adapter extends XmlAdapter<QueryHistoryEntryJaxb, QueryHistoryEntry> {

        @Override
        public QueryHistoryEntry unmarshal(QueryHistoryEntryJaxb jaxb) throws Exception {
            if (jaxb == null) {
                return null;
            }

            QueryHistoryEntry entry = new QueryHistoryEntry(jaxb.query, jaxb.executedOn);
            return entry;
        }

        @Override
        public QueryHistoryEntryJaxb marshal(QueryHistoryEntry histObj) throws Exception {
            if (histObj == null) {
                return null;
            }
            QueryHistoryEntryJaxb jaxb = new QueryHistoryEntryJaxb();
            jaxb.executedOn = histObj.executedOnUtc;
            jaxb.query = histObj.query;
            return jaxb;
        }
    }
}

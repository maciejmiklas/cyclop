package org.cyclop.model;

import com.google.common.base.Objects;

import javax.annotation.concurrent.Immutable;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlTransient;
import javax.xml.bind.annotation.adapters.XmlAdapter;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;
import java.util.Date;

/**
 * @author Maciej Miklas
 */
@Immutable
@XmlJavaTypeAdapter(QueryHistoryEntry.Adapter.class)
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

    @XmlRootElement
    @XmlAccessorType(XmlAccessType.FIELD)
    public final static class QueryHistoryEntryJaxb {
        public CqlQuery query;
        public Date executedOn;

        @Override
        public String toString() {
            return Objects.toStringHelper(this)
                    .add("query", query)
                    .add("executedOn", executedOn)
                    .toString();
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
            jaxb.executedOn = histObj.executedOn;
            jaxb.query = histObj.query;
            return jaxb;
        }
    }
}

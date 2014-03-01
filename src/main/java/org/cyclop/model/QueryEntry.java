package org.cyclop.model;

import com.google.common.base.Objects;
import net.jcip.annotations.Immutable;
import org.joda.time.DateTime;
import org.joda.time.DateTimeZone;

import javax.validation.Valid;
import javax.validation.constraints.NotNull;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlTransient;
import javax.xml.bind.annotation.adapters.XmlAdapter;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;
import java.io.Serializable;

/**
 * Unique by query, sorted by date
 *
 * @author Maciej Miklas
 */
@Immutable
@XmlJavaTypeAdapter(QueryEntry.Adapter.class)
public final class QueryEntry implements Comparable<QueryEntry>, Serializable {

	@NotNull @Valid
	public final CqlQuery query;

	@NotNull
	public final DateTime executedOnUtc;

	public QueryEntry(CqlQuery query) {
		this(query, new DateTime().toDateTime(DateTimeZone.UTC));
	}

	public QueryEntry(CqlQuery query, DateTime executedOnUtc) {
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

	/** equals and hashCode are only on query to recognize the same queries */
	@Override
	public boolean equals(final Object obj) {
		if (obj == null || getClass() != obj.getClass()) {
			return false;
		}

		final QueryEntry other = (QueryEntry) obj;
		return java.util.Objects.equals(query, other.query);
	}

	@Override
	public int compareTo(QueryEntry o) {
		int compRes = o.executedOnUtc.compareTo(executedOnUtc);

		// make sure that entries with the same date are not removed if query is
		// different
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
	public final static class Adapter extends XmlAdapter<QueryHistoryEntryJaxb, QueryEntry> {

		@Override
		public QueryEntry unmarshal(QueryHistoryEntryJaxb jaxb) throws Exception {
			if (jaxb == null) {
				return null;
			}

			QueryEntry entry = new QueryEntry(jaxb.query, jaxb.executedOn);
			return entry;
		}

		@Override
		public QueryHistoryEntryJaxb marshal(QueryEntry histObj) throws Exception {
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

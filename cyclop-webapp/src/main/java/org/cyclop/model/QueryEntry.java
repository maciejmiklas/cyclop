/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.cyclop.model;

import java.io.Serializable;

import javax.validation.Valid;
import javax.validation.constraints.NotNull;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlTransient;
import javax.xml.bind.annotation.adapters.XmlAdapter;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

import net.jcip.annotations.Immutable;

import org.joda.time.DateTime;
import org.joda.time.DateTimeZone;

import com.google.common.base.Objects;

/**
 * Unique by query, sorted by date
 *
 * @author Maciej Miklas
 */
@Immutable
@XmlJavaTypeAdapter(QueryEntry.Adapter.class)
public final class QueryEntry implements Comparable<QueryEntry>, Serializable {

    @NotNull
    @Valid
    public final CqlQuery query;

    @NotNull
    @Valid
    public final DateTime executedOnUtc;

    public final long runTime;

    public QueryEntry(CqlQuery query, long runTime) {
	this(query, new DateTime().toDateTime(DateTimeZone.UTC), runTime);
    }

    public QueryEntry(CqlQuery query, DateTime executedOnUtc, long runTime) {
	this.query = query;
	this.executedOnUtc = executedOnUtc;
	this.runTime = runTime;
    }

    @Override
    public String toString() {
	return Objects.toStringHelper(this).add("query", query).add("executedOnUtc", executedOnUtc)
		.toString();
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
	public long runTime;

	public int resultsSize;

	private CqlQuery query;

	private DateTime executedOn;

	@Override
	public String toString() {
	    return Objects.toStringHelper(this).add("query", query).add("executedOn", executedOn)
		    .add("runTime", runTime).add("resultsSize", resultsSize).toString();
	}
    }

    @XmlTransient
    public final static class Adapter extends XmlAdapter<QueryHistoryEntryJaxb, QueryEntry> {

	@Override
	public QueryEntry unmarshal(QueryHistoryEntryJaxb jaxb) throws Exception {
	    if (jaxb == null) {
		return null;
	    }

	    QueryEntry entry = new QueryEntry(jaxb.query, jaxb.executedOn, jaxb.runTime);
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
	    jaxb.runTime = histObj.runTime;
	    return jaxb;
	}
    }
}

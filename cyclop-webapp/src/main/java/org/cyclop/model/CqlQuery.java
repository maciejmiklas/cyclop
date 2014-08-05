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

import javax.validation.constraints.NotNull;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlTransient;
import javax.xml.bind.annotation.adapters.XmlAdapter;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

import net.jcip.annotations.Immutable;

import com.google.common.base.Objects;

/** @author Maciej Miklas */
@Immutable
@XmlJavaTypeAdapter(CqlQuery.Adapter.class)
public final class CqlQuery extends CqlPart {

    @NotNull
    public final CqlQueryType type;

    public CqlQuery(CqlQueryType type, String cql) {
	super(cql);
	this.type = type;
    }

    @Override
    public String toString() {
	return Objects.toStringHelper(this).add("part", part).add("type", type).toString();
    }

    @XmlRootElement
    @XmlAccessorType(XmlAccessType.FIELD)
    public final static class CqlQueryJaxb {
	public String cql;

	public CqlQueryType type;
    }

    @XmlTransient
    public final static class Adapter extends XmlAdapter<CqlQueryJaxb, CqlQuery> {

	@Override
	public CqlQuery unmarshal(CqlQueryJaxb jaxb) throws Exception {
	    if (jaxb == null) {
		return null;
	    }

	    CqlQuery entry = new CqlQuery(jaxb.type, jaxb.cql);
	    return entry;
	}

	@Override
	public CqlQueryJaxb marshal(CqlQuery histObj) throws Exception {
	    if (histObj == null) {
		return null;
	    }
	    CqlQueryJaxb jaxb = new CqlQueryJaxb();
	    jaxb.cql = histObj.part;
	    jaxb.type = histObj.type;
	    return jaxb;
	}
    }

    @edu.umd.cs.findbugs.annotations.SuppressWarnings("EQ_CHECK_FOR_OPERAND_NOT_COMPATIBLE_WITH_THIS")
    @Override
    public boolean equals(Object obj) {
	if (obj == null || getClass() != obj.getClass()) {
	    return false;
	}
	CqlQuery cqlObj = (CqlQuery) obj;
	return java.util.Objects.equals(partLc, cqlObj.partLc) && java.util.Objects.equals(type, cqlObj.type);
    }

    @Override
    public int hashCode() {
	return Objects.hashCode(partLc, type);
    }
}

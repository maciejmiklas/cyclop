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

import javax.validation.Valid;

import net.jcip.annotations.Immutable;

import com.google.common.base.Objects;

/**
 * Cql keyword
 *
 * @author Maciej Miklas
 */
@Immutable
public final class CqlTable extends CqlPart implements DisplaySupport {

    /** can be null */
    @Valid
    public final CqlKeySpace keySpace;

    public CqlTable(String keySpace, String table) {
	super(table);

	if (keySpace == null) {
	    this.keySpace = null;
	}
	else {
	    this.keySpace = new CqlKeySpace(keySpace);
	}
    }

    @Override
    public int hashCode() {
	return java.util.Objects.hash(partLc, keySpace);
    }

    @Override
    public boolean equals(Object obj) {
	if (obj == null || getClass() != obj.getClass()) {
	    return false;
	}
	final CqlTable other = (CqlTable) obj;
	return java.util.Objects.equals(partLc, other.partLc)
		&& java.util.Objects.equals(keySpace, other.keySpace);
    }

    public CqlTable(String table) {
	this(null, table);
    }

    @Override
    public int compareTo(CqlPart o) {
	if (o == null || getClass() != o.getClass()) {
	    return -1;
	}
	CqlTable table = (CqlTable) o;

	return toDisplayString().toLowerCase().compareTo(table.toDisplayString().toLowerCase());
    }

    public String toDisplayString() {
	StringBuilder buf = new StringBuilder();
	if (keySpace != null) {
	    buf.append(keySpace.part).append(".");
	}
	buf.append(part);
	return buf.toString();
    }

    @Override
    public String toString() {
	return Objects.toStringHelper(this).add("table", part).add("keySpace", keySpace).toString();
    }

    @Override
    public CqlType type() {
	return CqlType.TABLE;
    }
}

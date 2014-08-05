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
import javax.validation.constraints.NotNull;

import net.jcip.annotations.Immutable;

import com.google.common.base.Objects;

/** @author Maciej Miklas */
@Immutable
public final class CqlPartitionKeyValue extends CqlColumnValue {

    @NotNull
    @Valid
    public final CqlPartitionKey cqlPartitionKey;

    public CqlPartitionKeyValue(Class<?> valueClass, Object value, CqlPartitionKey cqlPartitionKey) {
	super(valueClass, value, cqlPartitionKey);
	this.cqlPartitionKey = cqlPartitionKey;
    }

    @Override
    public String toString() {
	return Objects.toStringHelper(this).add("valueClass", valueClass).add("prefix", value)
		.add("cqlPartitionKey", cqlPartitionKey).toString();
    }

    @Override
    public int hashCode() {
	return java.util.Objects.hash(cqlPartitionKey, valueClass, value, columnName);
    }

    @Override
    public boolean equals(Object obj) {
	if (obj == null || getClass() != obj.getClass()) {
	    return false;
	}
	final CqlPartitionKeyValue other = (CqlPartitionKeyValue) obj;
	return java.util.Objects.equals(cqlPartitionKey, other.cqlPartitionKey)
		&& java.util.Objects.equals(valueClass, other.valueClass)
		&& java.util.Objects.equals(value, other.value)
		&& java.util.Objects.equals(columnName, other.columnName);
    }
}

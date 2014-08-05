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
import java.util.List;

import javax.validation.Valid;
import javax.validation.constraints.NotNull;

import net.jcip.annotations.Immutable;

import org.apache.commons.lang.Validate;

import com.datastax.driver.core.DataType;
import com.datastax.driver.core.DataType.Name;

@Immutable
public final class CqlDataType implements Serializable {

    public final static CqlDataType TEXT = CqlDataType.create(DataType.text());

    /** type name */
    @NotNull
    public final DataType.Name name;

    /**
     * dataType.getTypeArguments().get(0) - set only for maps - it's the map key
     */
    @Valid
    public final Class<?> keyClass;

    /**
     * dataType.getTypeArguments().get(1) - set only for sets and maps. For set
     * it's set class, for map its value class
     */
    @Valid
    public final Class<?> valueClass;

    private final boolean collection;

    CqlDataType(Name name, Class<?> keyClass, Class<?> valueClass, boolean collection) {
	this.name = name;
	this.keyClass = keyClass;
	this.valueClass = valueClass;
	this.collection = collection;
    }

    public boolean isCollection() {
	return collection;
    }

    public boolean isUUID() {
	return name == DataType.uuid().getName() || name == DataType.timeuuid().getName();
    }

    public boolean isLong() {
	return name == DataType.bigint().getName() || name == DataType.counter().getName();
    }

    public boolean isString() {
	return name == DataType.ascii().getName()
		|| name == DataType.text().getName()
		|| name == DataType.varchar().getName();
    }

    public static CqlDataType create(DataType dataType) {
	Validate.notNull(dataType, "Null DataType");
	Class<?> keyClass = null;
	Class<?> valueClass = null;

	List<DataType> argTypes = dataType.getTypeArguments();
	if (argTypes != null) {
	    if (argTypes.size() >= 1) {
		keyClass = argTypes.get(0).asJavaClass();
	    }
	    if (argTypes.size() >= 2) {
		valueClass = argTypes.get(1).asJavaClass();
	    }
	}

	CqlDataType dt = new CqlDataType(dataType.getName(), keyClass, valueClass, dataType.isCollection());
	return dt;
    }

    @Override
    public String toString() {
	return "CqlDataType [name=" + name + ", keyClass=" + keyClass + ", valueClass=" + valueClass + "]";
    }

    @Override
    public int hashCode() {
	return java.util.Objects.hash(name, keyClass, valueClass);
    }

    @Override
    public boolean equals(final Object obj) {
	if (obj == null || getClass() != obj.getClass()) {
	    return false;
	}

	final CqlDataType other = (CqlDataType) obj;
	return java.util.Objects.equals(name, other.name)
		&& java.util.Objects.equals(keyClass, other.keyClass)
		&& java.util.Objects.equals(valueClass, other.valueClass);
    }

}

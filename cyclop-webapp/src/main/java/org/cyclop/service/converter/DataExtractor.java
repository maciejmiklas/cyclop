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
package org.cyclop.service.converter;

import java.util.Collection;
import java.util.Map;

import javax.inject.Named;
import javax.validation.constraints.NotNull;

import org.cyclop.model.CqlColumnValue;
import org.cyclop.model.CqlDataType;
import org.cyclop.model.CqlExtendedColumnName;
import org.cyclop.model.CqlPartitionKey;
import org.cyclop.model.CqlPartitionKeyValue;
import org.cyclop.validation.EnableValidation;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.datastax.driver.core.DataType;
import com.datastax.driver.core.Row;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableMap;

/** @author Maciej Miklas */
@Named
@EnableValidation
public class DataExtractor {
    private final static Logger LOG = LoggerFactory.getLogger(DataExtractor.class);

    public @NotNull ImmutableList<CqlColumnValue> extractCollection(
	    @NotNull Row row,
	    @NotNull CqlExtendedColumnName column) {
	String partLc = column.partLc;
	CqlDataType dataType = column.dataType;
	if (dataType.name != DataType.Name.SET && dataType.name != DataType.Name.LIST) {
	    throw new IllegalArgumentException("Only Collection type is supported");
	}

	if (dataType.keyClass == null) {
	    return ImmutableList.of();
	}

	Collection<?> objCont = dataType.name == DataType.Name.SET ? row.getSet(partLc, dataType.keyClass)
		: row.getList(partLc, dataType.keyClass);

	ImmutableList.Builder<CqlColumnValue> builder = ImmutableList.builder();
	for (Object o : objCont) {
	    CqlColumnValue dob = new CqlColumnValue(dataType.keyClass, o, column);
	    builder.add(dob);

	}
	ImmutableList<CqlColumnValue> collection = builder.build();

	LOG.trace("Extracted collection: {}", collection);
	return collection;
    }

    public @NotNull ImmutableMap<CqlColumnValue, CqlColumnValue> extractMap(
	    @NotNull Row row,
	    @NotNull CqlExtendedColumnName column) {
	String partLc = column.partLc;
	CqlDataType dataType = column.dataType;
	if (dataType.name != DataType.Name.MAP) {
	    throw new IllegalArgumentException("Only Map type is supported");
	}

	if (dataType.keyClass == null || dataType.valueClass == null) {
	    return ImmutableMap.of();
	}

	Map<?, ?> unconverted = row.getMap(partLc, dataType.keyClass, dataType.valueClass);

	ImmutableMap.Builder<CqlColumnValue, CqlColumnValue> builder = ImmutableMap.builder();
	for (Map.Entry<?, ?> entry : unconverted.entrySet()) {
	    CqlColumnValue dk = new CqlColumnValue(dataType.keyClass, entry.getKey(), column);
	    CqlColumnValue dv = new CqlColumnValue(dataType.valueClass, entry.getValue(), column);
	    builder.put(dk, dv);
	}

	ImmutableMap<CqlColumnValue, CqlColumnValue> map = builder.build();
	LOG.trace("Extracted map: {}", map);
	return map;
    }

    public @NotNull CqlPartitionKeyValue extractPartitionKey(
	    @NotNull Row row,
	    @NotNull CqlPartitionKey partitionKey) {
	CqlColumnValue colSv = extractSingleValue(row, partitionKey);
	CqlPartitionKeyValue key = new CqlPartitionKeyValue(colSv.valueClass, colSv.value, partitionKey);
	LOG.trace("Extracted: {}", key);
	return key;
    }

    public @NotNull CqlColumnValue extractSingleValue(@NotNull Row row, @NotNull CqlExtendedColumnName column) {
	String partLc = column.partLc;
	CqlDataType dataType = column.dataType;
	if (dataType.isCollection()) {
	    throw new IllegalArgumentException("Collection type is not supported");
	}

	Object extracted = null;
	if (dataType.isUUID()) {
	    extracted = row.getUUID(partLc);

	}
	else if (dataType.isString()) {
	    extracted = row.getString(partLc);

	}
	else if (dataType.isLong()) {
	    extracted = row.getLong(partLc);

	}
	else if (dataType.name == DataType.cfloat().getName()) {
	    extracted = row.getFloat(partLc);

	}
	else if (dataType.name == DataType.cint().getName()) {
	    extracted = row.getInt(partLc);

	}
	else if (dataType.name == DataType.cboolean().getName()) {
	    extracted = row.getBool(partLc);

	}
	else if (dataType.name == DataType.decimal().getName()) {
	    extracted = row.getDecimal(partLc);

	}
	else if (dataType.name == DataType.cdouble().getName()) {
	    extracted = row.getDouble(partLc);

	}
	else if (dataType.name == DataType.varint().getName()) {
	    extracted = row.getVarint(partLc);

	}
	else if (dataType.name == DataType.timestamp().getName()) {
	    extracted = row.getDate(partLc);

	}
	else if (dataType.name == DataType.inet().getName()) {
	    extracted = row.getInet(partLc);
	}
	else {
	    extracted = "?? " + column.part + " ??";
	    LOG.warn("Type: " + dataType + " not supported by data converter");
	}

	if (extracted == null) {
	    extracted = "";
	}

	Class<?> eClass = extracted.getClass();
	CqlColumnValue cqlColumnValue = new CqlColumnValue(eClass, extracted, column);

	LOG.debug("Extracted: {}", cqlColumnValue);
	return cqlColumnValue;
    }

}

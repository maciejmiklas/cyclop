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

import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.Serializable;
import java.util.Iterator;

import javax.validation.Valid;
import javax.validation.constraints.NotNull;

import org.apache.commons.collections4.iterators.EmptyIterator;
import org.cyclop.common.SerializationUtil;

import com.datastax.driver.core.Row;
import com.google.common.base.Objects;

/** @author Maciej Miklas */
public class CqlQueryResult implements Iterable<Row>, Serializable {
    public final static CqlQueryResult EMPTY = new CqlQueryResult();

    @NotNull
    @Valid
    public final CqlRowMetadata rowMetadata;

    @NotNull
    private final transient Iterator<Row> rows;

    @SuppressWarnings("unchecked")
    CqlQueryResult() {
	rows = EmptyIterator.INSTANCE;
	rowMetadata = CqlRowMetadata.EMPTY;
    }

    public CqlQueryResult(Iterator<Row> rowsIt, CqlRowMetadata rowMetadata) {
	this.rows = rowsIt;
	this.rowMetadata = rowMetadata;
    }

    @Override
    public Iterator<Row> iterator() {
	return rows;
    }

    private void readObject(ObjectInputStream in) throws ClassNotFoundException, IOException {
	in.defaultReadObject();
	SerializationUtil.setField(this, "rows", EmptyIterator.INSTANCE);
    }

    @Override
    public String toString() {
	return Objects.toStringHelper(this).add("rowMetadata", rowMetadata).toString();
    }

    public boolean isEmpty() {
	return !rows.hasNext();
    }
}

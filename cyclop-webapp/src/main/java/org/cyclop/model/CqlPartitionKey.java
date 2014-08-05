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

import java.util.Objects;

import net.jcip.annotations.Immutable;

/** @author Maciej Miklas */
@Immutable
public final class CqlPartitionKey extends CqlExtendedColumnName {

    protected CqlPartitionKey(CqlDataType dataType, String columnName) {
	super(CqlColumnType.PARTITION_KEY, dataType, columnName);
    }

    public static CqlPartitionKey fromColumn(CqlExtendedColumnName col) {
	return new CqlPartitionKey(col.dataType, col.part);
    }

    @Override
    public String toString() {
	return com.google.common.base.Objects.toStringHelper(this).add("columnType", columnType)
		.add("part", part).add("dataType", dataType).toString();
    }

    @Override
    public int hashCode() {
	return Objects.hash(partLc, columnType, dataType);
    }

    @Override
    public boolean equals(Object obj) {
	if (obj == null || getClass() != obj.getClass()) {
	    return false;
	}
	final CqlExtendedColumnName other = (CqlExtendedColumnName) obj;
	return Objects.equals(partLc, other.partLc)
		&& Objects.equals(columnType, other.columnType)
		&& Objects.equals(dataType, other.dataType);
    }

}

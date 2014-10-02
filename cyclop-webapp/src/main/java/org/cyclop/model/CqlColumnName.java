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

import javax.validation.Valid;
import javax.validation.constraints.NotNull;

import net.jcip.annotations.Immutable;

import com.datastax.driver.core.DataType;
import com.google.common.base.MoreObjects;

/** @author Maciej Miklas */
@Immutable
public class CqlColumnName extends CqlPart {

	@NotNull
	@Valid
	public CqlDataType dataType;

	public CqlColumnName(CqlDataType dataType, String columnName) {
		super(columnName);
		this.dataType = dataType;
	}

	public CqlColumnName(String columnName) {
		this(CqlDataType.create(DataType.text()), columnName);
	}

	@Override
	public boolean equals(Object obj) {
		if (obj == null || getClass() != obj.getClass()) {
			return false;
		}
		final CqlColumnName other = (CqlColumnName) obj;
		return Objects.equals(partLc, other.partLc) && Objects.equals(dataType, other.dataType);
	}

	@Override
	public int hashCode() {
		return Objects.hash(partLc, dataType);
	}

	@Override
	public String toString() {
		return MoreObjects.toStringHelper(this).add("part", part).add("dataType", dataType).toString();
	}

	@Override
	public CqlType type() {
		return CqlType.COLUMN;
	}
}

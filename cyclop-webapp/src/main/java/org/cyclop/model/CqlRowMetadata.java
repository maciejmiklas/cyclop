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

import com.google.common.collect.ImmutableList;
import net.jcip.annotations.Immutable;

import javax.validation.Valid;
import javax.validation.constraints.NotNull;
import java.io.Serializable;

// TODO test serialization
/** @author Maciej Miklas */
@Immutable
public final class CqlRowMetadata implements Serializable {

	public final static CqlRowMetadata EMPTY = new CqlRowMetadata();

	/** List of columns that value is not empty for multiple rows */
	@NotNull
	@Valid
	public final ImmutableList<CqlExtendedColumnName> commonColumns;

	/** List of columns that value is not empty only for single row, or it's empty */
	@NotNull
	@Valid
	public final ImmutableList<CqlExtendedColumnName> dynamicColumns;

	/** all columns returned by db query in exact the same order */
	@NotNull
	@Valid
	public final ImmutableList<CqlExtendedColumnName> columns;

	/** could be null, it not found in result, or in case of error while reading meta data info */
	public final transient CqlPartitionKey partitionKey;

	public CqlRowMetadata(ImmutableList<CqlExtendedColumnName> commonColumns,
						  ImmutableList<CqlExtendedColumnName> dynamicColumns,
						  ImmutableList<CqlExtendedColumnName> columns,
						  CqlPartitionKey partitionKey) {
		this.commonColumns = commonColumns;
		this.dynamicColumns = dynamicColumns;
		this.columns = columns;
		this.partitionKey = partitionKey;
	}


	private CqlRowMetadata() {
		this.commonColumns = ImmutableList.of();
		this.dynamicColumns = commonColumns;
		this.columns = commonColumns;
		this.partitionKey = null;
	}
}

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
package org.cyclop.service.cassandra;

import com.google.common.collect.ImmutableSortedSet;
import org.cyclop.model.CqlColumnName;
import org.cyclop.model.CqlIndex;
import org.cyclop.model.CqlKeySpace;
import org.cyclop.model.CqlQuery;
import org.cyclop.model.CqlQueryResult;
import org.cyclop.model.CqlTable;

import javax.validation.Valid;
import javax.validation.constraints.NotNull;

/** @author Maciej Miklas */
public interface QueryService {

	@NotNull
	ImmutableSortedSet<CqlColumnName> findColumnNames(@Valid CqlTable table);

	boolean checkTableExists(CqlTable table);

	@NotNull
	ImmutableSortedSet<CqlColumnName> findAllColumnNames();

	@NotNull
	ImmutableSortedSet<CqlIndex> findAllIndexes(@Valid CqlKeySpace keySpace);

	@NotNull
	ImmutableSortedSet<CqlKeySpace> findAllKeySpaces();

	@NotNull
	CqlQueryResult execute(@NotNull CqlQuery query);

	void executeSimple(@NotNull CqlQuery query, boolean updateHistory);

	@NotNull
	CqlQueryResult execute(@NotNull CqlQuery query, boolean updateHistory);

	@NotNull
	ImmutableSortedSet<CqlTable> findTableNames(@Valid CqlKeySpace keySpace);
}

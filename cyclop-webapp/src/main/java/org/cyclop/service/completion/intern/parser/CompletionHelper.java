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
package org.cyclop.service.completion.intern.parser;

import java.util.Optional;

import javax.inject.Inject;
import javax.inject.Named;

import org.cyclop.common.QueryHelper;
import org.cyclop.model.CqlCompletion;
import org.cyclop.model.CqlKeySpace;
import org.cyclop.model.CqlKeyword;
import org.cyclop.model.CqlQuery;
import org.cyclop.model.CqlTable;
import org.cyclop.service.cassandra.QueryScope;
import org.cyclop.service.cassandra.QueryService;

import com.google.common.collect.ImmutableSortedSet;

/** @author Maciej Miklas */
@Named
public final class CompletionHelper {

	@Inject
	private QueryService queryService;

	@Inject
	private QueryScope queryScope;

	public CqlCompletion.Builder computeTableNameCompletion(CqlQuery query, CqlKeyword... kw) {
		CqlCompletion.Builder completion = computeTableNameCompletionWithKeyspaceInQuery(query, kw).orElse(
				computeTableNameCompletionWithoutKeyspaceInQuery());
		return completion;
	}

	private Optional<CqlCompletion.Builder> computeTableNameCompletionWithKeyspaceInQuery(CqlQuery query,
			CqlKeyword... kw) {
		Optional<CqlKeySpace> keySpace = QueryHelper.extractKeyspace(query, kw);
		if (!keySpace.isPresent()) {
			return Optional.empty();
		}
		ImmutableSortedSet<CqlTable> tables = queryService.findTableNames(keySpace);
		if (tables.isEmpty()) {
			return Optional.empty();
		}

		CqlCompletion.Builder builder = CqlCompletion.Builder.naturalOrder();
		builder.all(tables);

		for (CqlTable ta : tables) {
			builder.full(new CqlTable(keySpace.get().part, ta.part));
		}

		return Optional.of(builder);
	}

	private CqlCompletion.Builder computeTableNameCompletionWithoutKeyspaceInQuery() {
		CqlCompletion.Builder builder = CqlCompletion.Builder.naturalOrder();

		Optional<CqlKeySpace> activeKeySpace = queryScope.getActiveKeySpace();
		ImmutableSortedSet<CqlTable> tables = queryService.findTableNames(activeKeySpace);
		builder.all(tables);

		ImmutableSortedSet<CqlKeySpace> keyspaces = queryService.findAllKeySpaces();
		for (CqlKeySpace ks : keyspaces) {
			builder.min(ks);
			builder.full(new CqlKeySpace(ks.partLc + "."));
		}

		return builder;
	}

}

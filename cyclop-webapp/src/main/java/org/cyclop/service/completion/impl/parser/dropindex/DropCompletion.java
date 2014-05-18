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
package org.cyclop.service.completion.impl.parser.dropindex;

import com.google.common.base.Objects;
import com.google.common.collect.ImmutableSortedSet;
import org.cyclop.model.CqlCompletion;
import org.cyclop.model.CqlIndex;
import org.cyclop.model.CqlKeySpace;
import org.cyclop.model.CqlKeyword;
import org.cyclop.model.CqlQuery;
import org.cyclop.service.cassandra.QueryScope;
import org.cyclop.service.cassandra.QueryService;
import org.cyclop.service.completion.impl.parser.OffsetBasedCompletion;

import javax.annotation.PostConstruct;
import javax.inject.Inject;
import javax.inject.Named;

/** @author Maciej Miklas */
@Named("dropindex.DropCompletion")
class DropCompletion implements OffsetBasedCompletion {

	private CqlCompletion.BuilderTemplate completion;

	@Inject
	private QueryService queryService;

	@Inject
	private QueryScope queryScope;

	@PostConstruct
	public void init() {
		completion = CqlCompletion.Builder.naturalOrder().all(CqlKeyword.Def.IF_NOT_EXISTS.value).template();
	}

	@Override
	public CqlCompletion getCompletion(CqlQuery query) {
		CqlKeySpace activeKeySpace = queryScope.getActiveKeySpace();
		ImmutableSortedSet<CqlIndex> allIndexesForActiveKeySpace = queryService.findAllIndexes(activeKeySpace);
		return completion.naturalOrder().all(allIndexesForActiveKeySpace).build();
	}

	@Override
	public int canApply(CqlQuery query, int queryPosition) {
		String cqlLc = query.part;
		int cqlLcLen = query.partLc.length();

		int indCreate = cqlLc.indexOf(CqlKeyword.Def.DROP_INDEX.value + " ", queryPosition);
		if (indCreate + 1 >= cqlLcLen) {
			return -1;
		}

		int indLastSpace = cqlLc.indexOf(' ', indCreate + 1);

		return indLastSpace;
	}

	@Override
	public String toString() {
		return Objects.toStringHelper(this).add("completion", completion).toString();
	}
}

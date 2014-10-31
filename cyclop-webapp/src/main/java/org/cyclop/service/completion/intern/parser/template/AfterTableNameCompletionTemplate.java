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
package org.cyclop.service.completion.intern.parser.template;

import java.util.Optional;

import javax.inject.Inject;
import javax.inject.Named;

import org.apache.commons.lang.Validate;
import org.cyclop.common.QueryHelper;
import org.cyclop.model.CqlKeyword;
import org.cyclop.model.CqlQuery;
import org.cyclop.model.CqlTable;
import org.cyclop.service.cassandra.QueryService;
import org.cyclop.service.completion.intern.parser.OffsetBasedCompletion;

/** @author Maciej Miklas */
@Named
public abstract class AfterTableNameCompletionTemplate implements OffsetBasedCompletion {

	@Inject
	private QueryService queryService;

	private CqlKeyword cqlKeyword;

	public AfterTableNameCompletionTemplate(CqlKeyword cqlKeyword) {
		Validate.notNull(cqlKeyword, "Null cqlKeyword");
		this.cqlKeyword = cqlKeyword;
	}

	@Override
	public final int canApply(CqlQuery query, int queryPosition) {
		Optional<CqlTable> tableOpt = QueryHelper.extractTableName(cqlKeyword, query);
		if (!tableOpt.isPresent()) {
			return -1;
		}

		int index = -1;
		CqlTable table = tableOpt.get();
		if (queryService.checkTableExists(table)) {
			index = query.partLc.indexOf(table.partLc) + table.partLc.length();
		}
		return index;
	}
}

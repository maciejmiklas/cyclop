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
package org.cyclop.service.completion.impl.parser.delete;

import static org.cyclop.common.QueryHelper.extractTableName;

import java.util.Optional;
import java.util.SortedSet;

import javax.annotation.PostConstruct;
import javax.inject.Inject;
import javax.inject.Named;

import org.cyclop.model.CqlColumnName;
import org.cyclop.model.CqlCompletion;
import org.cyclop.model.CqlKeyword;
import org.cyclop.model.CqlQuery;
import org.cyclop.model.CqlTable;
import org.cyclop.service.cassandra.QueryService;
import org.cyclop.service.completion.impl.parser.MarkerBasedCompletion;

/** @author Maciej Miklas */
@Named("delete.OrderByCompletion")
class OrderByCompletion extends MarkerBasedCompletion {

	@Inject
	private QueryService queryService;

	private CqlCompletion.BuilderTemplate builderTemplate;

	public OrderByCompletion() {
		super(CqlKeyword.Def.ORDER_BY.value);
	}

	@PostConstruct
	public void init() {
		builderTemplate = CqlCompletion.Builder.naturalOrder().all(CqlKeyword.Def.ASC.value)
				.all(CqlKeyword.Def.DESC.value).all(CqlKeyword.Def.LIMIT.value)
				.all(CqlKeyword.Def.ALLOW_FILTERING.value).all(CqlKeyword.Def.TOKEN.value).template();
	}

	@Override
	public CqlCompletion getCompletion(CqlQuery query) {
		CqlCompletion.Builder builder = builderTemplate.naturalOrder();

		Optional<CqlTable> table = extractTableName(CqlKeyword.Def.FROM.value, query);
		SortedSet<CqlColumnName> columnNames = queryService.findColumnNames(table);
		builder.all(columnNames);

		CqlCompletion cmp = builder.build();
		return cmp;
	}

}

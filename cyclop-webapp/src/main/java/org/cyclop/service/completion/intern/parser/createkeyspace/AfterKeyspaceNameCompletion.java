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
package org.cyclop.service.completion.intern.parser.createkeyspace;

import javax.annotation.PostConstruct;
import javax.inject.Named;

import org.cyclop.model.CqlCompletion;
import org.cyclop.model.CqlKeyword;
import org.cyclop.model.CqlQuery;
import org.cyclop.service.completion.intern.parser.OffsetBasedCompletion;

/** @author Maciej Miklas */
@Named("createkeyspace.AfterKeyspaceNameCompletion")
class AfterKeyspaceNameCompletion implements OffsetBasedCompletion {

	private CqlCompletion completion;

	@PostConstruct
	public void init() {
		completion = CqlCompletion.Builder.naturalOrder().all(CqlKeyword.Def.WITH.value)
				.all(CqlKeyword.Def20.IF_NOT_EXISTS.value).build();
	}

	@Override
	public CqlCompletion getCompletion(CqlQuery query) {
		return completion;
	}

	@Override
	public int canApply(CqlQuery query, int queryPosition) {
		String cqlLc = query.part;
		int cqlLcLen = query.partLc.length();

		int indCreate = cqlLc.indexOf(CqlKeyword.Def.CREATE_KEYSPACE.value + " ", queryPosition);
		if (indCreate + 1 >= cqlLcLen) {
			return -1;
		}

		int indLastSpace = cqlLc.indexOf(' ', indCreate + 1);

		return indLastSpace;
	}

}

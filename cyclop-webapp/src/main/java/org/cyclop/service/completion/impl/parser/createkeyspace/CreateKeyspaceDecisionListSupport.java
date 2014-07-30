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
package org.cyclop.service.completion.impl.parser.createkeyspace;

import org.cyclop.model.CqlKeyword;
import org.cyclop.model.CqlQueryType;
import org.cyclop.service.completion.impl.parser.CqlPartCompletion;
import org.cyclop.service.completion.impl.parser.DecisionListSupport;

import javax.annotation.PostConstruct;
import javax.inject.Inject;
import javax.inject.Named;

/** @author Maciej Miklas */
@Named
public class CreateKeyspaceDecisionListSupport implements DecisionListSupport {

	private final CqlKeyword supports = CqlKeyword.Def.CREATE_KEYSPACE.value;

	private CqlPartCompletion[][] decisionList;

	@Inject
	private AfterKeyspaceNameCompletion afterKeyspaceNameCompletion;

	@Inject
	private WithCompletion withCompletion;

	@PostConstruct
	public void init() {
		decisionList = new CqlPartCompletion[][]{{afterKeyspaceNameCompletion}, {withCompletion}};
	}

	@Override
	public CqlPartCompletion[][] getDecisionList() {
		return decisionList;
	}

	@Override
	public CqlKeyword beginnsWith() {
		return supports;
	}

	@Override
	public CqlQueryType queryName() {
		return CqlQueryType.CREATE_KEYSPACE;
	}

}

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
package org.cyclop.service.completion.intern.parser.dropkeyspace;

import javax.annotation.PostConstruct;
import javax.inject.Inject;
import javax.inject.Named;

import org.cyclop.model.CqlCompletion;
import org.cyclop.model.CqlKeySpace;
import org.cyclop.model.CqlKeyword;
import org.cyclop.model.CqlQuery;
import org.cyclop.service.cassandra.QueryService;
import org.cyclop.service.completion.intern.parser.MarkerBasedCompletion;

import com.google.common.collect.ImmutableSortedSet;

/** @author Maciej Miklas */
@Named("dropkeyspace.DropCompletion")
class DropCompletion extends MarkerBasedCompletion {

	@Inject
	private QueryService queryService;

	private CqlCompletion.BuilderTemplate completion;

	@PostConstruct
	public void init() {
		completion = CqlCompletion.Builder.naturalOrder().all(CqlKeyword.Def20.IF_EXISTS.value).template();
	}

	public DropCompletion() {
		super(CqlKeyword.Def.DROP_KEYSPACE.value);
	}

	@Override
	public CqlCompletion getCompletion(CqlQuery query) {
		ImmutableSortedSet<CqlKeySpace> keySpaces = queryService.findAllKeySpaces();
		return completion.naturalOrder().all(keySpaces).build();
	}

}

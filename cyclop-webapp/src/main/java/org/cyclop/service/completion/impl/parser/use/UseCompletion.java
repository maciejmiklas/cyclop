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
package org.cyclop.service.completion.impl.parser.use;

import com.google.common.base.Objects;
import com.google.common.collect.ImmutableSortedSet;
import org.cyclop.model.CqlCompletion;
import org.cyclop.model.CqlKeySpace;
import org.cyclop.model.CqlKeyword;
import org.cyclop.model.CqlQuery;
import org.cyclop.service.cassandra.QueryService;
import org.cyclop.service.completion.impl.parser.MarkerBasedCompletion;

import javax.inject.Inject;
import javax.inject.Named;

/** @author Maciej Miklas */
@Named("use.UsePartCompletion")
class UseCompletion extends MarkerBasedCompletion {

	@Inject
	private QueryService queryService;

	public UseCompletion() {
		super(CqlKeyword.Def.USE.value);
	}

	@Override
	public CqlCompletion getCompletion(CqlQuery query) {
		ImmutableSortedSet<CqlKeySpace> keySpaces = queryService.findAllKeySpaces();
		return CqlCompletion.Builder.naturalOrder().all(keySpaces).build();
	}

	@Override
	public String toString() {
		return Objects.toStringHelper(this).toString();
	}

}

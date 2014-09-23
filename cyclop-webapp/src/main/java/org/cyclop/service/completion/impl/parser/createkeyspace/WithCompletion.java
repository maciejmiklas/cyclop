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

import javax.inject.Named;

import org.cyclop.model.CqlCompletion;
import org.cyclop.model.CqlKeyword;
import org.cyclop.model.CqlKeywordValue;
import org.cyclop.service.completion.impl.parser.template.StaticMarkerBasedCompletion;

import com.google.common.base.Objects;

/** @author Maciej Miklas */
@Named("createkeyspace.WithCompletion")
class WithCompletion extends StaticMarkerBasedCompletion {

	public WithCompletion() {
		super(CqlKeyword.Def.WITH.value, CqlCompletion.Builder.naturalOrder().all(CqlKeyword.Def.AND.value)
				.all(CqlKeyword.Def.REPLICATION.value).all(CqlKeywordValue.Def.DURABLE_WRITES.value)
				.prefix(CqlKeywordValue.Def.CLASS.value).prefix(CqlKeywordValue.Def.SIMPLE_STRATEGY.value)
				.prefix(CqlKeywordValue.Def.REPLICATION_FACTOR.value)
				.prefix(CqlKeywordValue.Def.NETWORK_TOPOLOGY_STRATEGY.value)
				.prefix(CqlKeywordValue.Def.DURABLE_WRITES.value).all(CqlKeywordValue.Def.TRUE.value)
				.all(CqlKeywordValue.Def.FALSE.value).prefix(CqlKeywordValue.Def.OLD_NETWORK_TOPOLOGY_STRATEGY.value)
				.build());
	}

	@Override
	public String toString() {
		return Objects.toStringHelper(this).toString();
	}

}

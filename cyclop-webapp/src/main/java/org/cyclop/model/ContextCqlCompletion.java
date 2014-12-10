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
package org.cyclop.model;

import java.io.Serializable;
import java.util.Objects;

import javax.validation.Valid;
import javax.validation.constraints.NotNull;

import net.jcip.annotations.Immutable;

import com.google.common.base.MoreObjects;
import com.google.common.collect.ImmutableSortedSet;

/** @author Maciej Miklas */
@Immutable
public final class ContextCqlCompletion implements Serializable {

	public final static ContextCqlCompletion EMPTY = new ContextCqlCompletion(CqlQueryType.UNKNOWN,
			CqlCompletion.Builder.naturalOrder().build());
	@NotNull
	@Valid
	public final CqlQueryType queryName;

	@NotNull
	@Valid
	public final CqlCompletion cqlCompletion;

	public ContextCqlCompletion(CqlQueryType queryName, CqlCompletion cqlCompletion) {
		this.queryName = queryName;
		this.cqlCompletion = cqlCompletion;
	}

	public ContextCqlCompletion copyFromCompletion(ImmutableSortedSet<? extends CqlPart> fullCompletion,
			ImmutableSortedSet<? extends CqlPart> minCompletion) {

		CqlCompletion cqlCompletion = new CqlCompletion(fullCompletion, minCompletion);
		ContextCqlCompletion context = new ContextCqlCompletion(queryName, cqlCompletion);
		return context;
	}

	public boolean isEmpty() {
		return cqlCompletion.isEmpty();
	}

	@Override
	public int hashCode() {
		return Objects.hash(queryName, cqlCompletion);
	}

	@Override
	public boolean equals(Object obj) {
		if (obj == null || getClass() != obj.getClass()) {
			return false;
		}
		final ContextCqlCompletion other = (ContextCqlCompletion) obj;
		return Objects.equals(queryName, other.queryName) && Objects.equals(cqlCompletion, other.cqlCompletion);
	}

	@Override
	public String toString() {
		return MoreObjects.toStringHelper(this).add("queryName", queryName).add("cqlCompletion", cqlCompletion)
				.toString();
	}
}

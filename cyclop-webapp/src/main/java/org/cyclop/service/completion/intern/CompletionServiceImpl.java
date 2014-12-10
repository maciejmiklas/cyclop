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
package org.cyclop.service.completion.intern;

import java.util.Optional;
import java.util.function.Predicate;

import javax.inject.Inject;
import javax.inject.Named;

import net.jcip.annotations.ThreadSafe;

import org.cyclop.common.Gullectors;
import org.cyclop.model.ContextCqlCompletion;
import org.cyclop.model.CqlKeyword;
import org.cyclop.model.CqlPart;
import org.cyclop.model.CqlQuery;
import org.cyclop.model.CqlQueryType;
import org.cyclop.service.cassandra.CassandraSession;
import org.cyclop.service.completion.CompletionService;
import org.cyclop.service.completion.intern.parser.CqlParser;
import org.cyclop.validation.EnableValidation;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.common.collect.ImmutableSortedSet;

/** @author Maciej Miklas */
@Named
@ThreadSafe
@EnableValidation
class CompletionServiceImpl implements CompletionService {

	private final static Logger LOG = LoggerFactory.getLogger(CompletionServiceImpl.class);

	@Inject
	private CqlParser parser;

	@Inject
	private CassandraSession cassandraSession;

	@Override
	public ContextCqlCompletion findInitialCompletion() {
		ContextCqlCompletion compl = new ContextCqlCompletion(CqlQueryType.UNKNOWN, parser.findInitialCompletion());

		LOG.debug("Found initial completion: {}", compl);

		ContextCqlCompletion filtered = filterCompletion(compl);
		LOG.debug("Filtered completion: {}", filtered);
		return filtered;
	}

	@Override
	public ContextCqlCompletion findCompletion(CqlQuery cqlQuery) {
		ContextCqlCompletion compl = findCompletion(cqlQuery, -1);
		return compl;
	}

	@Override
	public ContextCqlCompletion findCompletion(CqlQuery cqlQuery, int cursorPosition) {
		if (cursorPosition == 0 || cursorPosition == 1) {
			return findInitialCompletion();
		}
		Optional<ContextCqlCompletion> fcomp = parser.findCompletion(cqlQuery, cursorPosition);
		ContextCqlCompletion comp = fcomp.orElse(ContextCqlCompletion.EMPTY);
		LOG.debug("Found completion for query {} -> {} - > {}", cqlQuery, cursorPosition, comp);

		ContextCqlCompletion filtered = filterCompletion(comp);
		LOG.debug("Filtered completion: {}", filtered);
		return filtered;
	}

	private ContextCqlCompletion filterCompletion(ContextCqlCompletion compl) {
		if (compl.cqlCompletion.isEmpty()) {
			return compl;
		}

		ImmutableSortedSet<? extends CqlPart> fullCompletion = filterVersion(compl.cqlCompletion.fullCompletion);
		ImmutableSortedSet<? extends CqlPart> minCompletion = filterVersion(compl.cqlCompletion.minCompletion);
		ContextCqlCompletion context = compl.copyFromCompletion(fullCompletion, minCompletion);
		return context;
	}

	private ImmutableSortedSet<? extends CqlPart> filterVersion(ImmutableSortedSet<? extends CqlPart> compl) {
		Predicate<CqlPart> casVerPred = q -> {
			if (q instanceof CqlKeyword) {
				CqlKeyword qw = (CqlKeyword) q;
				return cassandraSession.getCassandraVersion().within(qw.validFrom, qw.validTo);
			} else {
				return true;
			}
		};

		ImmutableSortedSet<? extends CqlPart> filtered = compl.stream().filter(casVerPred)
				.collect(Gullectors.toNaturalImmutableSortedSet());
		return filtered;
	}

}

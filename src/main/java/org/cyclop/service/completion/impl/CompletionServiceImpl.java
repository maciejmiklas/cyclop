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
package org.cyclop.service.completion.impl;

import net.jcip.annotations.ThreadSafe;
import org.cyclop.model.ContextCqlCompletion;
import org.cyclop.model.CqlCompletion;
import org.cyclop.model.CqlQuery;
import org.cyclop.model.CqlQueryType;
import org.cyclop.service.completion.CompletionService;
import org.cyclop.service.completion.impl.parser.CqlParser;
import org.cyclop.validation.EnableValidation;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.inject.Inject;
import javax.inject.Named;

/** @author Maciej Miklas */
@Named
@ThreadSafe
@EnableValidation
class CompletionServiceImpl implements CompletionService {

	private final static Logger LOG = LoggerFactory.getLogger(CompletionServiceImpl.class);

	@Inject
	private CqlParser parser;

	@Override
	public ContextCqlCompletion findInitialCompletion() {
		ContextCqlCompletion compl = new ContextCqlCompletion(CqlQueryType.UNKNOWN, parser.findInitialCompletion());

		LOG.debug("Found initial completion: {}", compl);
		return compl;
	}

	@Override
	public ContextCqlCompletion findCompletion(CqlQuery cqlQuery) {
		ContextCqlCompletion compl = findCompletion(cqlQuery, -1);
		LOG.debug("Found completion for query {} - > {}", cqlQuery, compl);
		return compl;
	}

	@Override
	public ContextCqlCompletion findCompletion(CqlQuery cqlQuery, int cursorPosition) {
		if (cursorPosition == 0 || cursorPosition == 1) {
			return findInitialCompletion();
		}
		ContextCqlCompletion comp = parser.findCompletion(cqlQuery, cursorPosition);
		if (comp == null) {
			comp = new ContextCqlCompletion(CqlQueryType.UNKNOWN, CqlCompletion.Builder.naturalOrder().build());
		}
		LOG.debug("Found completion for query {} -> {} - > {}", cqlQuery, cursorPosition, comp);
		return comp;
	}
}

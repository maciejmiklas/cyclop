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
package org.cyclop.service.completion.intern.parser;

import java.util.List;
import java.util.Optional;

import javax.annotation.PostConstruct;
import javax.inject.Inject;
import javax.inject.Named;

import org.cyclop.model.ContextCqlCompletion;
import org.cyclop.model.CqlCompletion;
import org.cyclop.model.CqlQuery;
import org.cyclop.model.CqlQueryType;
import org.cyclop.model.exception.ServiceException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * LL(1) like cql parser
 *
 * @author Maciej Miklas
 */
@Named
public class CqlParser {
	private final static Logger LOG = LoggerFactory.getLogger(CqlParser.class);

	@Inject
	private List<DecisionListSupport> decisionListSupportDef;

	private CqlCompletion initialCqlCompletion = null;

	@PostConstruct
	public void init() {

		CqlCompletion.Builder cb = CqlCompletion.Builder.naturalOrder();
		decisionListSupportDef.forEach(cf -> cb.all(cf.beginnsWith()));
		initialCqlCompletion = cb.build();

		LOG.debug("Initial completion {}", initialCqlCompletion);
	}

	private Optional<DecisionListSupport> findCompletionDecision(CqlQuery query) {
		Optional<DecisionListSupport> found = decisionListSupportDef.stream().filter(d -> d.supports(query))
				.findFirst();
		LOG.debug("Found Decision List for query {} -> {}", query, found);
		return found;
	}

	public CqlCompletion findInitialCompletion() {
		return initialCqlCompletion;
	}

	public Optional<ContextCqlCompletion> findCompletion(CqlQuery cqlQuery, int cursorPosition) {
		LOG.debug("Find completion for {} on {}", cqlQuery, cursorPosition);
		if (cursorPosition == -1) {
			cursorPosition = cqlQuery.part.length() - 1;
		}

		cursorPosition++;

		Optional<DecisionListSupport> dlsOp = findCompletionDecision(cqlQuery);
		if (!dlsOp.isPresent()) {
			// user started typing, has first world and there is no decision
			// list for it
			ContextCqlCompletion initial = null;
			if (!cqlQuery.partLc.isEmpty() && !cqlQuery.partLc.contains(" ")) {
				initial = new ContextCqlCompletion(CqlQueryType.UNKNOWN, initialCqlCompletion);
			}
			return Optional.ofNullable(initial);
		}

		if (cursorPosition < 0) {
			cursorPosition = 0;
		}

		DecisionListSupport dls = dlsOp.get();
		CqlPartCompletion[][] decisionList = dls.getDecisionList();
		if (decisionList.length == 0) {
			return Optional.empty();
		}

		int cqLength = cqlQuery.partLc.length();
		if (cursorPosition > cqLength) {
			cursorPosition = cqLength;
		}

		String cqlLc = cqlQuery.partLc.substring(0, cursorPosition);
		int offset = 0;
		CqlPartCompletion lastMatchingCompletion = null;

		// go over all parsing decisions, until you find one that cannot be
		// applied - this means that previous one
		// is the right chose for completion
		for (CqlPartCompletion[] partCompletionList : decisionList) {
			LOG.debug("Next completion");
			boolean found = false;
			for (CqlPartCompletion partCompletion : partCompletionList) {
				LOG.debug("Checking: {}", partCompletion);
				int completionStartMarker = -1;
				if (partCompletion instanceof MarkerBasedCompletion) {
					MarkerBasedCompletion partStatic = (MarkerBasedCompletion) partCompletion;
					String startMarker = partStatic.startMarker().partLc;
					completionStartMarker = cqlLc.indexOf(startMarker, offset);

				} else if (partCompletion instanceof OffsetBasedCompletion) {
					OffsetBasedCompletion partDynamic = (OffsetBasedCompletion) partCompletion;
					completionStartMarker = partDynamic.canApply(cqlQuery, offset);

				} else {
					throw new ServiceException("Unsupported CqlPartCompletion: " + partCompletion.getClass());
				}

				LOG.debug("completionStartMarker: {}", completionStartMarker);

				if (completionStartMarker == -1) {
					// this decision cannot be applied - try next one
					continue;
				}
				found = true;
				// current decision can be applied - try next one
				offset = completionStartMarker + 1;
				lastMatchingCompletion = partCompletion;
			}

			if (!found) {
				break;
			}
		}
		ContextCqlCompletion cqc = null;
		if (lastMatchingCompletion != null) {
			CqlCompletion cqlCompletion = lastMatchingCompletion.getCompletion(cqlQuery);
			cqc = new ContextCqlCompletion(dls.queryName(), cqlCompletion);
			LOG.debug("Completion found: {}", cqc);
		} else {
			LOG.debug("Completion not found");
		}

		return Optional.ofNullable(cqc);
	}

}

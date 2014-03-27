package org.cyclop.service.completion.impl.parser;

import org.cyclop.model.ContextCqlCompletion;
import org.cyclop.model.CqlCompletion;
import org.cyclop.model.CqlQuery;
import org.cyclop.model.CqlQueryType;
import org.cyclop.model.exception.ServiceException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.annotation.PostConstruct;
import javax.inject.Inject;
import javax.inject.Named;
import java.util.List;

/**
 * LL(1) like cql parser
 *
 * @author Maciej Miklas
 */
@Named
public class CqlParser {
	private final static Logger LOG = LoggerFactory.getLogger(CqlParser.class);

	@Inject
	private List<DecisionListSupport> decisionListFactoryList;

	private CqlCompletion initialCqlCompletion = null;

	@PostConstruct
	public void init() {

		CqlCompletion.Builder cb = CqlCompletion.Builder.naturalOrder();
		for (DecisionListSupport cf : decisionListFactoryList) {
			cb.all(cf.supports());
		}
		initialCqlCompletion = cb.build();

		LOG.debug("Initial completion {}", initialCqlCompletion);
	}

	private DecisionListSupport findCompletionDecisionList(CqlQuery query) {
		DecisionListSupport found = null;
		for (DecisionListSupport cf : decisionListFactoryList) {
			if (query.partLc.startsWith(cf.supports().partLc)) {
				found = cf;
				break;
			}
		}
		LOG.debug("Found Decision List for query {} -> {}", query, found);
		return found;
	}

	public CqlCompletion findInitialCompletion() {
		return initialCqlCompletion;
	}

	public ContextCqlCompletion findCompletion(CqlQuery cqlQuery, int cursorPosition) {
		LOG.debug("Find completion for {} on {}", cqlQuery, cursorPosition);
		if (cursorPosition == -1) {
			cursorPosition = cqlQuery.part.length() - 1;
		}

		cursorPosition++;

		DecisionListSupport dls = findCompletionDecisionList(cqlQuery);
		if (dls == null) {
			// user started typing, has first world and there is no decision
			// list for it
			if (!cqlQuery.partLc.isEmpty() && !cqlQuery.partLc.contains(" ")) {
				ContextCqlCompletion initial = new ContextCqlCompletion(CqlQueryType.UNKNOWN, initialCqlCompletion);
				return initial;
			}
			return null;
		}

		if (cursorPosition < 0) {
			cursorPosition = 0;
		}

		CqlPartCompletion[][] decisionList = dls.getDecisionList();
		if (decisionList == null || decisionList.length == 0) {
			return null;
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
					// next decision cannot be applied - so the last selected
					// one will be taken
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

		if (lastMatchingCompletion == null) {
			LOG.debug("Completion not found");
			return null;
		} else {
			CqlCompletion cqlCompletion = lastMatchingCompletion.getCompletion(cqlQuery);
			ContextCqlCompletion cqc = new ContextCqlCompletion(dls.queryName(), cqlCompletion);
			LOG.debug("Completion foudn: {}", cqc);
			return cqc;
		}
	}

}

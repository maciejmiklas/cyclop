package org.cyclop.service.completion.impl.parser.truncate;

import org.cyclop.model.CqlKeyword;
import org.cyclop.model.CqlQueryName;
import org.cyclop.service.completion.impl.parser.CqlPartCompletion;
import org.cyclop.service.completion.impl.parser.DecisionListSupport;

import javax.annotation.PostConstruct;
import javax.inject.Inject;
import javax.inject.Named;

/** @author Maciej Miklas */
@Named
class TruncateDecisionListSupport implements DecisionListSupport {

	private final CqlKeyword supports = CqlKeyword.Def.TRUNCATE.value;

	private CqlPartCompletion[][] decisionList;

	@Inject TruncateCompletion truncateCompletion;

	@PostConstruct
	public void init() {
		decisionList = new CqlPartCompletion[][]{{truncateCompletion}};
	}

	@Override
	public CqlPartCompletion[][] getDecisionList() {
		return decisionList;
	}

	@Override
	public CqlKeyword supports() {
		return supports;
	}

	@Override
	public CqlQueryName queryName() {
		return CqlQueryName.TRUNCATE;
	}

}

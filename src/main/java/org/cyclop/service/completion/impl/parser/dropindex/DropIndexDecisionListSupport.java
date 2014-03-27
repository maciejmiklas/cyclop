package org.cyclop.service.completion.impl.parser.dropindex;

import org.cyclop.model.CqlKeyword;
import org.cyclop.model.CqlQueryType;
import org.cyclop.service.completion.impl.parser.CqlPartCompletion;
import org.cyclop.service.completion.impl.parser.DecisionListSupport;

import javax.annotation.PostConstruct;
import javax.inject.Inject;
import javax.inject.Named;

/** @author Maciej Miklas */
@Named
class DropIndexDecisionListSupport implements DecisionListSupport {

	private final CqlKeyword supports = CqlKeyword.Def.DROP_INDEX.value;

	private CqlPartCompletion[][] decisionList;

	@Inject
	private DropCompletion dropCompletion;

	@PostConstruct
	public void init() {
		decisionList = new CqlPartCompletion[][]{{dropCompletion}};
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
	public CqlQueryType queryName() {
		return CqlQueryType.DROP_INDEX;
	}

}

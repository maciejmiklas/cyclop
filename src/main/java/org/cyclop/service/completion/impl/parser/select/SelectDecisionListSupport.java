package org.cyclop.service.completion.impl.parser.select;

import org.cyclop.model.CqlKeyword;
import org.cyclop.model.CqlQueryType;
import org.cyclop.service.completion.impl.parser.CqlPartCompletion;
import org.cyclop.service.completion.impl.parser.DecisionListSupport;

import javax.annotation.PostConstruct;
import javax.inject.Inject;
import javax.inject.Named;

/** @author Maciej Miklas */
@Named
class SelectDecisionListSupport implements DecisionListSupport {

	private CqlPartCompletion[][] decisionList;

	private final CqlKeyword supports = CqlKeyword.Def.SELECT.value;

	@Inject
	private SelectCompletionTemplate selectCompletion;

	@Inject
	private FromCompletion fromCompletion;

	@Inject
	private AfterFromCompletion afterFromCompletion;

	@Inject
	private WhereCompletion whereCompletion;

	@Inject
	private OrderByClausePartCompletion orderByClausePartCompletion;

	@PostConstruct
	public void init() {
		decisionList = new CqlPartCompletion[][]{{selectCompletion}, {fromCompletion}, {afterFromCompletion}, {whereCompletion, orderByClausePartCompletion}, {orderByClausePartCompletion}};
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
		return CqlQueryType.SELECT;
	}

}

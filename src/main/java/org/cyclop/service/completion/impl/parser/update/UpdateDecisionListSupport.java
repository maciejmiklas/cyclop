package org.cyclop.service.completion.impl.parser.update;

import org.cyclop.model.CqlKeyword;
import org.cyclop.model.CqlQueryType;
import org.cyclop.service.completion.impl.parser.CqlPartCompletion;
import org.cyclop.service.completion.impl.parser.DecisionListSupport;

import javax.annotation.PostConstruct;
import javax.inject.Inject;
import javax.inject.Named;

/** @author Maciej Miklas */
@Named
class UpdateDecisionListSupport implements DecisionListSupport {

	private final CqlKeyword supports = CqlKeyword.Def.UPDATE.value;

	@Inject
	private TableNameCompletion tableNameCompletion;

	@Inject
	private ColumnsCompletion columnsCompletion;

	@Inject
	private AfterUpdateCompletion afterUpdateCompletion;

	@Inject
	private WhereCompletion whereCompletion;

	private CqlPartCompletion[][] decisionList;

	@PostConstruct
	public void init() {
		decisionList = new CqlPartCompletion[][]{{tableNameCompletion}, {afterUpdateCompletion}, {columnsCompletion}, {whereCompletion}};
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
		return CqlQueryType.UPDATE;
	}

}

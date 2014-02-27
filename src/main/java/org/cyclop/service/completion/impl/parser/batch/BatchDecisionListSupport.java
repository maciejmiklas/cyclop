package org.cyclop.service.completion.impl.parser.batch;

import org.cyclop.model.CqlKeyword;
import org.cyclop.model.CqlNotSupported;
import org.cyclop.model.CqlQueryName;
import org.cyclop.service.completion.impl.parser.CqlPartCompletion;
import org.cyclop.service.completion.impl.parser.DecisionListSupport;

import javax.annotation.PostConstruct;
import javax.inject.Inject;
import javax.inject.Named;

/** @author Maciej Miklas */
@Named
class BatchDecisionListSupport implements DecisionListSupport {

	private final CqlKeyword supports = new CqlNotSupported("batch");

	private CqlPartCompletion[][] decisionList;

	@Inject BatchCompletion batchCompletion;

	@PostConstruct
	public void init() {
		decisionList = new CqlPartCompletion[][]{{batchCompletion}};
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
		return CqlQueryName.BATCH;
	}

}

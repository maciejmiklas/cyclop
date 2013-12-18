package org.cyclop.service.completion.parser.select;

import org.cyclop.model.CqlKeyword;
import org.cyclop.model.CqlQueryType;
import org.cyclop.service.completion.parser.CqlPartCompletion;
import org.cyclop.service.completion.parser.DecisionListSupport;

import javax.annotation.PostConstruct;
import javax.inject.Inject;
import javax.inject.Named;

/**
 * @author Maciej Miklas
 */
@Named
public class SelectDecisionListSupport implements DecisionListSupport {

    private CqlPartCompletion[] decisionList;

    private final CqlKeyword supports = new CqlKeyword("select");

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
        decisionList = new CqlPartCompletion[]{selectCompletion, fromCompletion,
                afterFromCompletion, whereCompletion, orderByClausePartCompletion};
    }

    @Override
    public CqlPartCompletion[] getDecisionList() {
        return decisionList;
    }

    @Override
    public CqlKeyword supports() {
        return supports;
    }

    @Override
    public CqlQueryType queryType() {
        return CqlQueryType.SELECT;
    }

}

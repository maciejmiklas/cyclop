package org.cyclop.service.completion.parser.decisionlist.select;

import javax.annotation.PostConstruct;
import javax.inject.Inject;
import javax.inject.Named;
import org.cyclop.service.completion.parser.decisionlist.CqlPartCompletion;
import org.cyclop.service.completion.parser.decisionlist.DecisionListSupport;
import org.cyclop.service.model.CqlKeyword;
import org.cyclop.service.model.CqlQueryType;

/**
 * @author Maciej Miklas
 */
@Named
public class SelectDecisionListSupport implements DecisionListSupport {

    private CqlPartCompletion[] decisionList;

    private final CqlKeyword supports = new CqlKeyword("select");

    @Inject
    private SelectClausePartCompletion selectClausePartCompletion;

    @Inject
    private FromClausePartCompletion fromClausePartCompletion;

    @Inject
    private AfterFromClausePartCompletion afterFromClausePartCompletion;

    @Inject
    private WhereClausePartCompletion whereClausePartCompletion;

    @Inject
    private OrderByClausePartCompletion orderByClausePartCompletion;

    @PostConstruct
    public void init() {
        decisionList = new CqlPartCompletion[]{selectClausePartCompletion, fromClausePartCompletion,
                afterFromClausePartCompletion, whereClausePartCompletion, orderByClausePartCompletion};
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

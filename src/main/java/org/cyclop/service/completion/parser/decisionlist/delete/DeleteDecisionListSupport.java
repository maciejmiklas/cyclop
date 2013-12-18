package org.cyclop.service.completion.parser.decisionlist.delete;

import org.cyclop.model.CqlKeyword;
import org.cyclop.model.CqlQueryType;
import org.cyclop.service.completion.parser.decisionlist.CqlPartCompletion;
import org.cyclop.service.completion.parser.decisionlist.DecisionListSupport;

import javax.annotation.PostConstruct;
import javax.inject.Inject;
import javax.inject.Named;

/**
 * @author Maciej Miklas
 */
@Named
public class DeleteDecisionListSupport implements DecisionListSupport {

    private final CqlKeyword supports = new CqlKeyword("delete");

    private CqlPartCompletion[] decisionList;

    @Inject
    private DeleteClauseCompletion deleteClauseCompletion;

    @Inject
    private FromClauseCompletion fromClauseCompletion;

    @Inject
    private AfterFromCompletion afterFromCompletion;

    @Inject
    private WhereClauseCompletion whereClauseCompletion;

    @Inject
    private OrderByCompletion orderByCompletion;

    @PostConstruct
    public void init() {
        decisionList = new CqlPartCompletion[]{deleteClauseCompletion, fromClauseCompletion,
                afterFromCompletion, whereClauseCompletion, orderByCompletion};
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
        return CqlQueryType.DELETE;
    }

}

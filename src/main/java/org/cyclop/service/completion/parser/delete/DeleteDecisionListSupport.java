package org.cyclop.service.completion.parser.delete;

import org.cyclop.model.CqlKeyword;
import org.cyclop.model.CqlQueryName;
import org.cyclop.service.completion.parser.CqlPartCompletion;
import org.cyclop.service.completion.parser.DecisionListSupport;

import javax.annotation.PostConstruct;
import javax.inject.Inject;
import javax.inject.Named;

/**
 * @author Maciej Miklas
 */
@Named
public class DeleteDecisionListSupport implements DecisionListSupport {

    private final CqlKeyword supports = CqlKeyword.Def.DELETE.value;

    private CqlPartCompletion[][] decisionList;

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
        decisionList = new CqlPartCompletion[][]{{deleteClauseCompletion}, {fromClauseCompletion}, {afterFromCompletion},
                {whereClauseCompletion}, {orderByCompletion}};
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
        return CqlQueryName.DELETE;
    }

}

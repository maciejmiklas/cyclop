package org.cyclop.service.completion.parser.decisionlist.droptable;

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
@Named("droptable.DropTableDecisionListSupport")
public class DropTableDecisionListSupport implements DecisionListSupport {

    private final CqlKeyword supports = new CqlKeyword("drop table");

    private CqlPartCompletion[] decisionList;

    @Inject
    DropCompletion dropCompletion;

    @PostConstruct
    public void init() {
        decisionList = new CqlPartCompletion[]{dropCompletion};
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
        return CqlQueryType.DROP_TABLE;
    }

}

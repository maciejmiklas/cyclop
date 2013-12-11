package org.cyclop.service.completion.parser.decisionlist.droptable;

import javax.annotation.PostConstruct;
import javax.inject.Inject;
import javax.inject.Named;
import org.cyclop.service.completion.parser.decisionlist.CqlPartCompletion;
import org.cyclop.service.completion.parser.decisionlist.DecisionListSupport;
import org.cyclop.service.model.CqlKeyword;
import org.cyclop.service.model.CqlNotSupported;
import org.cyclop.service.model.CqlQueryType;

// TODO html help

/**
 * @author Maciej Miklas
 */
@Named("droptable.DropTableDecisionListSupport")
public class DropTableDecisionListSupport implements DecisionListSupport {

    private final CqlKeyword supports = new CqlNotSupported("drop table");

    private CqlPartCompletion[] decisionList;

    @Inject
    DropPartCompletion dropPartCompletion;

    @PostConstruct
    public void init() {
        decisionList = new CqlPartCompletion[]{dropPartCompletion};
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

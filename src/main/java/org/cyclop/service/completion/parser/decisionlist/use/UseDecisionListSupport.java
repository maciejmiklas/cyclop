package org.cyclop.service.completion.parser.decisionlist.use;

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
public class UseDecisionListSupport implements DecisionListSupport {

    private final CqlKeyword supports = new CqlKeyword("use");

    private CqlPartCompletion[] decisionList;

    @Inject
    UsePartCompletion usePartCompletion;

    @PostConstruct
    public void init() {
        decisionList = new CqlPartCompletion[]{usePartCompletion};
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
        return CqlQueryType.USE;
    }

}

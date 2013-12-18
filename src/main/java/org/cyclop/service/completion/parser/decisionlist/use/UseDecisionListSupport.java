package org.cyclop.service.completion.parser.decisionlist.use;

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
public class UseDecisionListSupport implements DecisionListSupport {

    private final CqlKeyword supports = new CqlKeyword("use");

    private CqlPartCompletion[] decisionList;

    @Inject
    UseCompletion useCompletion;

    @PostConstruct
    public void init() {
        decisionList = new CqlPartCompletion[]{useCompletion};
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

package org.cyclop.service.completion.parser.decisionlist.delete;

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
@Named
public class DeleteDecisionListSupport implements DecisionListSupport {

    private final CqlKeyword supports = new CqlNotSupported("delete");

    private CqlPartCompletion[] decisionList;

    @Inject
    DeletePartCompletion deletePartCompletion;

    @PostConstruct
    public void init() {
        decisionList = new CqlPartCompletion[]{deletePartCompletion};
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

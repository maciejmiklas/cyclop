package org.cyclop.service.completion.parser.decisionlist.createtable;

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
public class CreateTableDecisionListSupport implements DecisionListSupport {

    private final CqlKeyword supports = new CqlNotSupported("create table");

    private CqlPartCompletion[] decisionList;

    @Inject
    CreatePartCompletion createPartCompletion;

    @PostConstruct
    public void init() {
        decisionList = new CqlPartCompletion[]{createPartCompletion};
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
        return CqlQueryType.CREATE_TABLE;
    }

}

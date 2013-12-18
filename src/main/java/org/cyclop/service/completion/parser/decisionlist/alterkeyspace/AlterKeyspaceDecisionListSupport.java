package org.cyclop.service.completion.parser.decisionlist.alterkeyspace;

import org.cyclop.model.CqlKeyword;
import org.cyclop.model.CqlNotSupported;
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
public class AlterKeyspaceDecisionListSupport implements DecisionListSupport {

    private final CqlKeyword supports = new CqlNotSupported("alter keyspace");

    private CqlPartCompletion[] decisionList;

    @Inject
    AlterCompletion alterCompletion;

    @PostConstruct
    public void init() {
        decisionList = new CqlPartCompletion[]{alterCompletion};
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
        return CqlQueryType.ALTER_KEYSPACE;
    }

}

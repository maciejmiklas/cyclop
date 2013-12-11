package org.cyclop.service.completion.parser.decisionlist.alterkeyspace;

import javax.annotation.PostConstruct;
import javax.inject.Inject;
import javax.inject.Named;
import org.cyclop.service.completion.parser.decisionlist.CqlPartCompletion;
import org.cyclop.service.completion.parser.decisionlist.DecisionListSupport;
import org.cyclop.service.model.CqlKeyword;
import org.cyclop.service.model.CqlNotSupported;
import org.cyclop.service.model.CqlQueryType;

/**
 * @author Maciej Miklas
 */
@Named
public class AlterKeyspaceDecisionListSupport implements DecisionListSupport {

    private final CqlKeyword supports = new CqlNotSupported("alter keyspace");

    private CqlPartCompletion[] decisionList;

    @Inject
    AlterPartCompletion alterPartCompletion;

    @PostConstruct
    public void init() {
        decisionList = new CqlPartCompletion[]{alterPartCompletion};
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

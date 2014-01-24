package org.cyclop.service.completion.impl.parser.createindex;

import javax.annotation.PostConstruct;
import javax.inject.Inject;
import javax.inject.Named;
import org.cyclop.model.CqlKeyword;
import org.cyclop.model.CqlNotSupported;
import org.cyclop.model.CqlQueryName;
import org.cyclop.service.completion.impl.parser.CqlPartCompletion;
import org.cyclop.service.completion.impl.parser.DecisionListSupport;

/**
 * @author Maciej Miklas
 */
@Named
class CreateIndexDecisionListSupport implements DecisionListSupport {

    private final CqlKeyword supports = new CqlNotSupported("create index");

    private CqlPartCompletion[][] decisionList;

    @Inject
    CreateCompletion createCompletion;

    @PostConstruct
    public void init() {
        decisionList = new CqlPartCompletion[][]{{createCompletion}};
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
        return CqlQueryName.CREATE_INDEX;
    }

}

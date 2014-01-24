package org.cyclop.service.completion.impl.parser.dropindex;

import javax.annotation.PostConstruct;
import javax.inject.Inject;
import javax.inject.Named;
import org.cyclop.model.CqlKeyword;
import org.cyclop.model.CqlQueryName;
import org.cyclop.service.completion.impl.parser.CqlPartCompletion;
import org.cyclop.service.completion.impl.parser.DecisionListSupport;

/**
 * @author Maciej Miklas
 */
@Named
class DropIndexDecisionListSupport implements DecisionListSupport {

    private final CqlKeyword supports = CqlKeyword.Def.DROP_INDEX.value;

    private CqlPartCompletion[][] decisionList;

    @Inject
    private DropCompletion dropCompletion;

    @PostConstruct
    public void init() {
        decisionList = new CqlPartCompletion[][]{{dropCompletion}};
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
        return CqlQueryName.DROP_INDEX;
    }

}

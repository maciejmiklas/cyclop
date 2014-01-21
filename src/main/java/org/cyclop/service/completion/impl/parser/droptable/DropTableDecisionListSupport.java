package org.cyclop.service.completion.impl.parser.droptable;

import javax.annotation.concurrent.ThreadSafe;
import org.cyclop.model.CqlKeyword;
import org.cyclop.model.CqlQueryName;
import org.cyclop.service.completion.impl.parser.CqlPartCompletion;
import org.cyclop.service.completion.impl.parser.DecisionListSupport;

import javax.annotation.PostConstruct;
import javax.inject.Inject;
import javax.inject.Named;

/**
 * @author Maciej Miklas
 */
@Named("droptable.DropTableDecisionListSupport")
@ThreadSafe
public class DropTableDecisionListSupport implements DecisionListSupport {

    private final CqlKeyword supports = CqlKeyword.Def.DROP_TABLE.value;

    private CqlPartCompletion[][] decisionList;

    @Inject
    DropCompletion dropCompletion;

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
        return CqlQueryName.DROP_TABLE;
    }

}

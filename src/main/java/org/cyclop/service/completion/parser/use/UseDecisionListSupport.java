package org.cyclop.service.completion.parser.use;

import org.cyclop.model.CqlKeyword;
import org.cyclop.model.CqlQueryName;
import org.cyclop.service.completion.parser.CqlPartCompletion;
import org.cyclop.service.completion.parser.DecisionListSupport;

import javax.annotation.PostConstruct;
import javax.inject.Inject;
import javax.inject.Named;

/**
 * @author Maciej Miklas
 */
@Named
public class UseDecisionListSupport implements DecisionListSupport {

    private final CqlKeyword supports = CqlKeyword.Def.USE.value;

    private CqlPartCompletion[][] decisionList;

    @Inject
    UseCompletion useCompletion;

    @PostConstruct
    public void init() {
        decisionList = new CqlPartCompletion[][]{{useCompletion}};
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
        return CqlQueryName.USE;
    }

}

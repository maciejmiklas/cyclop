package org.cyclop.service.completion.parser.truncate;

import javax.annotation.PostConstruct;
import javax.inject.Inject;
import javax.inject.Named;
import org.cyclop.model.CqlKeyword;
import org.cyclop.model.CqlQueryName;
import org.cyclop.service.completion.parser.CqlPartCompletion;
import org.cyclop.service.completion.parser.DecisionListSupport;

/**
 * @author Maciej Miklas
 */
@Named
public class TruncateDecisionListSupport implements DecisionListSupport {

    private final CqlKeyword supports = CqlKeyword.Def.TRUNCATE.value;

    private CqlPartCompletion[][] decisionList;

    @Inject
    TruncateCompletion truncateCompletion;

    @PostConstruct
    public void init() {
        decisionList = new CqlPartCompletion[][]{{truncateCompletion}};
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
        return CqlQueryName.TRUNCATE;
    }

}

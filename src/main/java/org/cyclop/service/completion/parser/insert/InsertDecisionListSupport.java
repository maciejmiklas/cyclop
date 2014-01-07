package org.cyclop.service.completion.parser.insert;

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
public class InsertDecisionListSupport implements DecisionListSupport {

    private final CqlKeyword supports = CqlKeyword.Def.INSERT_INTO.value;

    @Inject
    private TableNameCompletion tableNameCompletion;

    @Inject
    private ColumnsCompletion columnsCompletion;

    @Inject
    private AfterColumnsCompletion afterColumnsCompletion;

    @Inject
    private AfterValuesPartCompletion afterValuesPartCompletion;

    private CqlPartCompletion[] decisionList;

    @PostConstruct
    public void init() {
        decisionList = new CqlPartCompletion[]{tableNameCompletion, columnsCompletion, afterColumnsCompletion,
                afterValuesPartCompletion};
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
    public CqlQueryName queryName() {
        return CqlQueryName.INSERT;
    }

}

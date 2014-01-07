package org.cyclop.service.completion.parser.update;

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
public class UpdateDecisionListSupport implements DecisionListSupport {

    private final CqlKeyword supports = CqlKeyword.Def.UPDATE.value;

    @Inject
    private TableNameCompletion tableNameCompletion;

    @Inject
    private ColumnsCompletion columnsCompletion;

    @Inject
    private AfterUpdateCompletion afterUpdateCompletion;

    @Inject
    private WhereCompletion whereCompletion;

    private CqlPartCompletion[] decisionList;

    @PostConstruct
    public void init() {
        decisionList = new CqlPartCompletion[]{tableNameCompletion, afterUpdateCompletion, columnsCompletion,
                whereCompletion};
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
        return CqlQueryName.UPDATE;
    }

}

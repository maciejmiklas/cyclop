package org.cyclop.service.completion.parser.decisionlist.update;

import org.cyclop.model.CqlKeyword;
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
public class UpdateDecisionListSupport implements DecisionListSupport {

    private final CqlKeyword supports = new CqlKeyword("update");

    @Inject
    private TableNameCompletion tableNameCompletion;

    @Inject
    private ColumnsCompletion columnsCompletion;

    @Inject
    private AfterUpdateCCompletion afterUpdateCCompletion;

    @Inject
    private WhereCompletion whereCompletion;

    private CqlPartCompletion[] decisionList;

    @PostConstruct
    public void init() {
        decisionList = new CqlPartCompletion[] { tableNameCompletion, afterUpdateCCompletion, columnsCompletion,
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
    public CqlQueryType queryType() {
        return CqlQueryType.UPDATE;
    }

}

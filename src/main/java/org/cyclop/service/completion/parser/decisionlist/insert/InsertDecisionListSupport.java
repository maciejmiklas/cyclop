package org.cyclop.service.completion.parser.decisionlist.insert;

import javax.annotation.PostConstruct;
import javax.inject.Inject;
import javax.inject.Named;
import org.cyclop.service.completion.parser.decisionlist.CqlPartCompletion;
import org.cyclop.service.completion.parser.decisionlist.DecisionListSupport;
import org.cyclop.service.model.CqlKeyword;
import org.cyclop.service.model.CqlQueryType;

/**
 * @author Maciej Miklas
 */
@Named
public class InsertDecisionListSupport implements DecisionListSupport {

    private final CqlKeyword supports = new CqlKeyword("insert into");

    @Inject
    private TableNamePartCompletion tableNamePartCompletion;

    @Inject
    private ColumnsPartCompletion columnsPartCompletion;

    @Inject
    private AfterColumnsPartCompletion afterColumnsPartCompletion;

    @Inject
    private AfterValuesPartCompletion afterValuesPartCompletion;

    private CqlPartCompletion[] decisionList;

    @PostConstruct
    public void init() {
        decisionList = new CqlPartCompletion[]{tableNamePartCompletion, columnsPartCompletion,
                afterColumnsPartCompletion, afterValuesPartCompletion};
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
        return CqlQueryType.INSERT;
    }

}

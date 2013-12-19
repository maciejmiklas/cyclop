package org.cyclop.service.completion.parser.insert;

import javax.inject.Inject;
import javax.inject.Named;
import org.cyclop.model.CqlCompletion;
import org.cyclop.model.CqlKeyword;
import org.cyclop.model.CqlQuery;
import org.cyclop.service.completion.parser.DecisionHelper;
import org.cyclop.service.completion.parser.MarkerBasedCompletion;

/**
 * @author Maciej Miklas
 */
@Named("insert.TableNameCompletion")
public class TableNameCompletion extends MarkerBasedCompletion {

    @Inject
    private DecisionHelper decisionHelper;

    public TableNameCompletion() {
        super(CqlKeyword.Def.INSERT_INTO.value);
    }

    @Override
    public CqlCompletion getCompletion(CqlQuery query) {
        CqlCompletion completion = decisionHelper.computeTableNameCompletion(CqlKeyword.Def.INSERT_INTO.value, query);
        return completion;
    }

}

package org.cyclop.service.completion.parser.update;

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
@Named("update.TableNameCompletion")
public class TableNameCompletion extends MarkerBasedCompletion {

    @Inject
    private DecisionHelper decisionHelper;

    public TableNameCompletion() {
        super(CqlKeyword.Def.UPDATE.value);
    }

    @Override
    public CqlCompletion getCompletion(CqlQuery query) {
        CqlCompletion completion = decisionHelper.computeTableNameCompletion(CqlKeyword.Def.UPDATE.value, query);
        return completion;
    }

}

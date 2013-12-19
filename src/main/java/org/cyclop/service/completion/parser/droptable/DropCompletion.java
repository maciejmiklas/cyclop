package org.cyclop.service.completion.parser.droptable;

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
@Named("droptable.DropCompletion")
public class DropCompletion extends MarkerBasedCompletion {

    @Inject
    private DecisionHelper decisionHelper;

    public DropCompletion() {
        super(CqlKeyword.Def.DROP.value);
    }

    @Override
    public CqlCompletion getCompletion(CqlQuery query) {
        CqlCompletion completion = decisionHelper.computeTableNameCompletion(CqlKeyword.Def.DROP.value, query);
        return completion;
    }

}

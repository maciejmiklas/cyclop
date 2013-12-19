package org.cyclop.service.completion.parser.select;

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
@Named("select.FromCompletion")
public class FromCompletion extends MarkerBasedCompletion {

    @Inject
    private DecisionHelper decisionHelper;

    public FromCompletion() {
        super(CqlKeyword.Def.FROM.value);
    }

    @Override
    public CqlCompletion getCompletion(CqlQuery query) {
        CqlCompletion completion = decisionHelper.computeTableNameCompletion(CqlKeyword.Def.FROM.value, query);
        return completion;
    }

}

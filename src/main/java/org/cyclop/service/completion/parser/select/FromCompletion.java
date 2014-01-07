package org.cyclop.service.completion.parser.select;

import javax.inject.Inject;
import javax.inject.Named;
import org.cyclop.model.CqlCompletion;
import org.cyclop.model.CqlKeyword;
import org.cyclop.model.CqlQuery;
import org.cyclop.service.completion.parser.CompletionHelper;
import org.cyclop.service.completion.parser.MarkerBasedCompletion;

/**
 * @author Maciej Miklas
 */
@Named("select.FromCompletion")
public class FromCompletion extends MarkerBasedCompletion {

    @Inject
    private CompletionHelper completionHelper;

    public FromCompletion() {
        super(CqlKeyword.Def.FROM.value);
    }

    @Override
    public CqlCompletion getCompletion(CqlQuery query) {
        CqlCompletion completion = completionHelper.computeTableNameCompletion(CqlKeyword.Def.FROM.value, query);
        return completion;
    }

}

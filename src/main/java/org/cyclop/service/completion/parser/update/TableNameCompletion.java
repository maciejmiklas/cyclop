package org.cyclop.service.completion.parser.update;

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
@Named("update.TableNameCompletion")
public class TableNameCompletion extends MarkerBasedCompletion {

    @Inject
    private CompletionHelper completionHelper;

    public TableNameCompletion() {
        super(CqlKeyword.Def.UPDATE.value);
    }

    @Override
    public CqlCompletion getCompletion(CqlQuery query) {
        CqlCompletion.Builder completion = completionHelper.computeTableNameCompletion(query, CqlKeyword.Def.UPDATE.value);
        return completion.build();
    }

}

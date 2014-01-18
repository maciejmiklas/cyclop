package org.cyclop.service.completion.parser.select;

import com.google.common.base.Objects;
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
        CqlCompletion.Builder completion = completionHelper.computeTableNameCompletion(query, CqlKeyword.Def.FROM.value);
        return completion.build();
    }

    @Override
    public String toString() {
        return Objects.toStringHelper(this).toString();
    }

}

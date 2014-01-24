package org.cyclop.service.completion.impl.parser.insert;

import com.google.common.base.Objects;
import javax.annotation.PostConstruct;
import javax.inject.Named;
import org.cyclop.model.CqlCompletion;
import org.cyclop.model.CqlKeyword;
import org.cyclop.model.CqlPart;
import org.cyclop.model.CqlQuery;
import org.cyclop.service.completion.impl.parser.MarkerBasedCompletion;

/**
 * @author Maciej Miklas
 */
@Named("insert.AfterValuesPartCompletion")
class AfterValuesPartCompletion extends MarkerBasedCompletion {

    private CqlCompletion completion = null;

    public AfterValuesPartCompletion() {
        super(new CqlPart(")"));
    }

    @PostConstruct
    public void init() {
        completion = CqlCompletion.Builder.naturalOrder().all(CqlKeyword.Def.USING_TTL.value)
                .all(CqlKeyword.Def.USING_TIMESTAMP.value).all(CqlKeyword.Def.AND.value).build();
    }

    @Override
    public CqlCompletion getCompletion(CqlQuery query) {
        return completion;
    }

    @Override
    public String toString() {
        return Objects.toStringHelper(this).toString();
    }

}

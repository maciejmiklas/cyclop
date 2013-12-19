package org.cyclop.service.completion.parser.insert;

import javax.annotation.PostConstruct;
import javax.inject.Named;
import org.cyclop.model.CqlCompletion;
import org.cyclop.model.CqlKeyword;
import org.cyclop.model.CqlPart;
import org.cyclop.model.CqlQuery;
import org.cyclop.service.completion.parser.MarkerBasedCompletion;

/**
 * @author Maciej Miklas
 */
@Named("insert.AfterColumnsCompletion")
public class AfterColumnsCompletion extends MarkerBasedCompletion {

    private CqlCompletion completion;

    public AfterColumnsCompletion() {
        super(new CqlPart(")"));
    }

    @PostConstruct
    public void init() {
        completion = CqlCompletion.Builder.naturalOrder().all(CqlKeyword.Def.VALUES.value).build();
    }

    @Override
    public CqlCompletion getCompletion(CqlQuery query) {
        return completion;
    }

}

package org.cyclop.service.completion.parser.insert;

import com.google.common.collect.ImmutableSortedSet;
import org.cyclop.model.CqlCompletion;
import org.cyclop.model.CqlKeyword;
import org.cyclop.model.CqlPart;
import org.cyclop.model.CqlQuery;
import org.cyclop.service.completion.parser.CqlPartCompletionStatic;

import javax.annotation.PostConstruct;
import javax.inject.Named;

/**
 * @author Maciej Miklas
 */
@Named("insert.AfterColumnsCompletion")
public class AfterColumnsCompletion implements CqlPartCompletionStatic {

    private CqlCompletion completion;

    private final static CqlPart SM = new CqlPart(")");


    @PostConstruct
    public void init() {
        completion = CqlCompletion.Builder.naturalOrder().all(new CqlKeyword("values")).build();
    }

    @Override
    public CqlCompletion getCompletion(CqlQuery query) {
        return completion;
    }

    @Override
    public CqlPart startMarker() {
        return SM;
    }

}

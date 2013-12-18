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
@Named("insert.AfterValuesPartCompletion")
public class AfterValuesPartCompletion implements CqlPartCompletionStatic {

    private final CqlPart SM = new CqlPart(")");

    private CqlCompletion completion = null;

    @PostConstruct
    public void init() {
        completion = CqlCompletion.Builder.naturalOrder().all(new CqlKeyword("using ttl")).
                all(new CqlKeyword("using timestamp")).all(new CqlKeyword("and")).build();
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


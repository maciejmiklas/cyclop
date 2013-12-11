package org.cyclop.service.completion.parser.decisionlist.use;

import com.google.common.collect.ImmutableSortedSet;
import javax.inject.Inject;
import javax.inject.Named;
import org.cyclop.service.cassandra.QueryService;
import org.cyclop.service.completion.parser.decisionlist.CqlPartCompletionStatic;
import org.cyclop.service.model.CqlCompletion;
import org.cyclop.service.model.CqlKeySpace;
import org.cyclop.service.model.CqlKeyword;
import org.cyclop.service.model.CqlPart;
import org.cyclop.service.model.CqlQuery;

/**
 * @author Maciej Miklas
 */
@Named("use.UsePartCompletion")
public class UsePartCompletion implements CqlPartCompletionStatic {

    private final CqlPart[] startMarker = new CqlPart[]{new CqlKeyword("use")};

    @Inject
    private QueryService queryService;

    @Override
    public CqlCompletion getCompletion(CqlQuery query) {
        ImmutableSortedSet<CqlKeySpace> keySpaces = queryService.findAllKeySpaces();
        CqlCompletion completion = new CqlCompletion(keySpaces, keySpaces);
        return completion;
    }

    @Override
    public CqlPart[] startMarkers() {
        return startMarker;
    }
}

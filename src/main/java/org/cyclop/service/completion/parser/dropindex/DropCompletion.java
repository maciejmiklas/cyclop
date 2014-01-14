package org.cyclop.service.completion.parser.dropindex;

import com.google.common.collect.ImmutableSortedSet;
import javax.annotation.PostConstruct;
import javax.inject.Inject;
import javax.inject.Named;
import org.cyclop.model.CqlCompletion;
import org.cyclop.model.CqlIndex;
import org.cyclop.model.CqlKeySpace;
import org.cyclop.model.CqlKeyword;
import org.cyclop.model.CqlQuery;
import org.cyclop.service.cassandra.QueryScope;
import org.cyclop.service.cassandra.QueryService;
import org.cyclop.service.completion.parser.OffsetBasedCompletion;

/**
 * @author Maciej Miklas
 */
@Named("dropindex.DropCompletion")
public class DropCompletion implements OffsetBasedCompletion {

    private CqlCompletion.BuilderTemplate completion;

    @Inject
    private QueryService queryService;

    @Inject
    private QueryScope queryScope;

    @PostConstruct
    public void init() {
        completion = CqlCompletion.Builder.naturalOrder().all(CqlKeyword.Def.IF_NOT_EXISTS.value).template();
    }

    @Override
    public CqlCompletion getCompletion(CqlQuery query) {
        CqlKeySpace activeKeySpace = queryScope.getActiveKeySpace();
        ImmutableSortedSet<CqlIndex> allIndexesForActiveKeySpace = queryService.findAllIndexes(activeKeySpace);
        return completion.naturalOrder().all(allIndexesForActiveKeySpace).build();
    }

    @Override
    public int canApply(CqlQuery query, int queryPosition) {
        String cqlLc = query.cqlLc;
        int cqlLcLen = query.cqlLc.length();

        int indCreate = cqlLc.indexOf(CqlKeyword.Def.DROP_INDEX.value + " ", queryPosition);
        if (indCreate + 1 >= cqlLcLen) {
            return -1;
        }

        int indLastSpace = cqlLc.indexOf(' ', indCreate + 1);

        return indLastSpace;
    }
}

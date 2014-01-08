package org.cyclop.service.completion.parser;

import com.google.common.collect.ImmutableSortedSet;
import javax.inject.Inject;
import javax.inject.Named;
import org.cyclop.common.QueryHelper;
import org.cyclop.model.CqlCompletion;
import org.cyclop.model.CqlKeySpace;
import org.cyclop.model.CqlKeyword;
import org.cyclop.model.CqlQuery;
import org.cyclop.model.CqlTable;
import org.cyclop.service.cassandra.QueryService;

/**
 * @author Maciej Miklas
 */
@Named
public final class CompletionHelper {

    @Inject
    private QueryService queryService;

    public CqlCompletion computeTableNameCompletion(CqlKeyword kw, CqlQuery query) {
        CqlCompletion completion = computeTableNameCompletionWithKeyspaceInQuery(kw, query);
        if (completion == null) {
            completion = computeTableNameCompletionWithoutKeyspaceInQuery();
        }
        return completion;
    }

    private CqlCompletion computeTableNameCompletionWithKeyspaceInQuery(CqlKeyword kw, CqlQuery query) {
        CqlKeySpace keySpace = QueryHelper.extractKeyspace(kw, query);
        if (keySpace == null) {
            return null;
        }
        ImmutableSortedSet<CqlTable> tables = queryService.findTableNames(keySpace);
        if (tables.isEmpty()) {
            return null;
        }

        CqlCompletion.Builder builder = CqlCompletion.Builder.naturalOrder();
        builder.all(tables);

        for (CqlTable ta : tables) {
            builder.full(new CqlKeySpace(keySpace.partLc + "." + ta.partLc));
        }

        CqlCompletion cmp = builder.build();
        return cmp;
    }

    private CqlCompletion computeTableNameCompletionWithoutKeyspaceInQuery() {
        CqlCompletion.Builder builder = CqlCompletion.Builder.naturalOrder();

        ImmutableSortedSet<CqlTable> tables = queryService.findTableNamesForActiveKeySpace();
        builder.all(tables);

        ImmutableSortedSet<CqlKeySpace> keyspaces = queryService.findAllKeySpaces();
        for (CqlKeySpace ks : keyspaces) {
            builder.min(ks);
            builder.full(new CqlKeySpace(ks.partLc + "."));
        }

        CqlCompletion cmp = builder.build();
        return cmp;
    }

}

package org.cyclop.service.completion.parser.update;

import com.google.common.collect.ImmutableSortedSet;
import org.cyclop.model.*;
import org.cyclop.service.cassandra.QueryService;
import org.cyclop.service.completion.parser.CqlPartCompletionStatic;

import javax.annotation.PostConstruct;
import javax.inject.Inject;
import javax.inject.Named;

import static org.cyclop.common.QueryHelper.extractTableName;
import static org.cyclop.model.CqlKeywords.UPDATE;

/**
 * @author Maciej Miklas
 */
@Named("update.ColumnsCompletion")
public class ColumnsCompletion implements CqlPartCompletionStatic {

    private final static CqlPart SM = new CqlPart("set");

    private ImmutableSortedSet<CqlPart> staticPart;

    @Inject
    private QueryService queryService;

    @PostConstruct
    public void init() {
        ImmutableSortedSet.Builder<CqlPart> completionBuild = ImmutableSortedSet.naturalOrder();
        completionBuild.add(new CqlKeyword("where"));
        staticPart = completionBuild.build();
    }

    @Override
    public CqlCompletion getCompletion(CqlQuery query) {

        CqlTable table = extractTableName(UPDATE, query);
        ImmutableSortedSet<CqlColumnName> columnNames = queryService.findColumnNames(table);

        ImmutableSortedSet.Builder<CqlPart> completionFullBuild = ImmutableSortedSet.naturalOrder();
        completionFullBuild.addAll(columnNames);
        completionFullBuild.addAll(staticPart);

        ImmutableSortedSet<CqlPart> completion = completionFullBuild.build();
        CqlCompletion cmp = new CqlCompletion(completion, completion);
        return cmp;
    }

    @Override
    public CqlPart startMarker() {
        return SM;
    }

}

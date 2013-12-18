package org.cyclop.service.completion.parser.update;

import com.google.common.collect.ImmutableSortedSet;
import org.cyclop.model.*;
import org.cyclop.service.cassandra.QueryService;
import org.cyclop.service.completion.parser.CqlPartCompletionStatic;

import javax.annotation.PostConstruct;
import javax.inject.Inject;
import javax.inject.Named;
import java.util.ArrayList;
import java.util.List;
import java.util.SortedSet;

import static org.cyclop.common.QueryHelper.extractTableName;
import static org.cyclop.model.CqlKeywords.UPDATE;

/**
 * @author Maciej Miklas
 */
@Named("update.WhereCompletion")
public class WhereCompletion implements CqlPartCompletionStatic {

    private final CqlPart SM = new CqlKeyword("where");

    @Inject
    private QueryService queryService;

    private final List<CqlPart> staticPartFull = new ArrayList<>();
    private final List<CqlPart> staticPartMin = new ArrayList<>();

    @PostConstruct
    public void init() {
        CqlKeyword and = new CqlKeyword("and");
        staticPartFull.add(and);
        staticPartMin.add(and);
    }

    @Override
    public CqlCompletion getCompletion(CqlQuery query) {
        CqlTable table = extractTableName(UPDATE, query);
        SortedSet<CqlColumnName> columnNames = queryService.findColumnNames(table);

        ImmutableSortedSet.Builder<CqlPart> completionFullBuild = ImmutableSortedSet.naturalOrder();
        completionFullBuild.addAll(columnNames);
        completionFullBuild.addAll(staticPartFull);

        ImmutableSortedSet.Builder<CqlPart> completionMinBuild = ImmutableSortedSet.naturalOrder();
        completionMinBuild.addAll(columnNames);
        completionMinBuild.addAll(staticPartMin);

        CqlCompletion cmp = new CqlCompletion(completionFullBuild.build(), completionMinBuild.build());
        return cmp;
    }

    @Override
    public CqlPart startMarker() {
        return SM;
    }

}

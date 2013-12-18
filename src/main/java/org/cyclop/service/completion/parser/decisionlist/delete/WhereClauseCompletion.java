package org.cyclop.service.completion.parser.decisionlist.delete;

import com.google.common.collect.ImmutableSortedSet;
import org.cyclop.model.*;
import org.cyclop.service.cassandra.QueryService;
import org.cyclop.service.completion.parser.decisionlist.CqlPartCompletionStatic;

import javax.annotation.PostConstruct;
import javax.inject.Inject;
import javax.inject.Named;
import java.util.ArrayList;
import java.util.List;
import java.util.SortedSet;

import static org.cyclop.common.QueryHelper.extractTableName;
import static org.cyclop.model.CqlKeywords.FROM;

/**
 * @author Maciej Miklas
 */
@Named("delete.WhereClauseCompletion")
public class WhereClauseCompletion implements CqlPartCompletionStatic {

    private final CqlPart[] startMarker = new CqlPart[]{new CqlKeyword("where")};

    @Inject
    private QueryService queryService;

    private final List<CqlPart> staticPartFull = new ArrayList<>();
    private final List<CqlPart> staticPartMin = new ArrayList<>();

    @PostConstruct
    public void init() {
        CqlKeyword and = new CqlKeyword("and");
        staticPartFull.add(and);
        staticPartMin.add(and);

        staticPartFull.add(new CqlKeyword("in ("));
        staticPartMin.add(new CqlKeyword("in"));

    }

    @Override
    public CqlCompletion getCompletion(CqlQuery query) {
        CqlTable table = extractTableName(FROM, query);
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
    public CqlPart[] startMarkers() {
        return startMarker;
    }

}

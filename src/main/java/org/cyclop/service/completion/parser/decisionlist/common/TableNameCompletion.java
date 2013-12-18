package org.cyclop.service.completion.parser.decisionlist.common;

import com.google.common.collect.ImmutableSortedSet;
import org.cyclop.model.*;
import org.cyclop.service.cassandra.CassandraSession;
import org.cyclop.service.cassandra.QueryService;
import org.cyclop.service.completion.parser.decisionlist.CqlPartCompletionStatic;

import javax.inject.Inject;
import javax.inject.Named;
import java.util.List;
import java.util.SortedSet;

import static org.cyclop.common.QueryHelper.extractTableName;

@Named
public abstract class TableNameCompletion implements CqlPartCompletionStatic {

    @Inject
    private QueryService queryService;

    private final CqlPart[] startMarker;

    private final List<CqlPart> staticPart;

    @Inject
    private CassandraSession session;

    private CqlKeywords cqlKeywords;

    public TableNameCompletion(List<CqlPart> staticPart, CqlPart[] startMarker, CqlKeywords cqlKeywords) {
        this.staticPart = staticPart;
        this.startMarker = startMarker;
        this.cqlKeywords = cqlKeywords;
    }


    @Override
    public final CqlCompletion getCompletion(CqlQuery query) {

        ImmutableSortedSet.Builder<CqlPart> completionBuild = ImmutableSortedSet.naturalOrder();
        completionBuild.addAll(staticPart);

        SortedSet<CqlColumnName> columnNames;
        CqlTable table = extractTableName(cqlKeywords, query);
        if (queryService.checkTableExists(table)) {
            columnNames = queryService.findColumnNames(table);
        } else {
            columnNames = queryService.findAllColumnNames();
        }

        completionBuild.addAll(columnNames);

        ImmutableSortedSet<CqlPart> completion = completionBuild.build();
        CqlCompletion cmp = new CqlCompletion(completion, completion);
        return cmp;
    }

    @Override
    public final CqlPart[] startMarkers() {
        return startMarker;
    }

}
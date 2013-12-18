package org.cyclop.service.completion.parser.template;

import com.google.common.collect.ImmutableSortedSet;
import org.cyclop.model.*;
import org.cyclop.service.cassandra.CassandraSession;
import org.cyclop.service.cassandra.QueryService;
import org.cyclop.service.completion.parser.CqlPartCompletionStatic;

import javax.inject.Inject;
import javax.inject.Named;
import java.util.List;
import java.util.SortedSet;

import static org.cyclop.common.QueryHelper.extractTableName;

@Named
public abstract class ColumnNameCompletionTemplate implements CqlPartCompletionStatic {

    @Inject
    private QueryService queryService;

    private final List<CqlPart> staticPart;

    @Inject
    private CassandraSession session;

    private CqlKeywords cqlKeywords;

    public ColumnNameCompletionTemplate(List<CqlPart> staticPart,  CqlKeywords cqlKeywords) {
        this.staticPart = staticPart;
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


}
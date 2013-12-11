package org.cyclop.service.completion.parser.decisionlist.select;

import com.google.common.collect.ImmutableSortedSet;
import java.util.ArrayList;
import java.util.List;
import java.util.SortedSet;
import javax.annotation.PostConstruct;
import javax.inject.Inject;
import javax.inject.Named;
import org.cyclop.service.cassandra.CassandraSession;
import org.cyclop.service.cassandra.QueryService;
import org.cyclop.service.completion.parser.decisionlist.CqlPartCompletionStatic;
import org.cyclop.service.model.CqlColumnName;
import org.cyclop.service.model.CqlCompletion;
import org.cyclop.service.model.CqlKeyword;
import org.cyclop.service.model.CqlPart;
import org.cyclop.service.model.CqlQuery;
import org.cyclop.service.model.CqlTable;

import static org.cyclop.common.QueryHelper.KW_SELECT;
import static org.cyclop.common.QueryHelper.extractTableName;

/**
 * @author Maciej Miklas
 */
@Named("select.SelectClausePartCompletion")
public class SelectClausePartCompletion implements CqlPartCompletionStatic {

    @Inject
    private QueryService queryService;

    private final CqlPart[] startMarker = new CqlPart[]{new CqlKeyword("select")};

    private final List<CqlPart> staticPart = new ArrayList<>();

    @Inject
    private CassandraSession session;

    @PostConstruct
    public void init() {
        staticPart.add(new CqlKeyword("count (*)"));
        staticPart.add(new CqlKeyword("count (1)"));
        staticPart.add(new CqlKeyword("writetime"));
        staticPart.add(new CqlKeyword("ttl"));
        staticPart.add(new CqlKeyword("from"));
    }

    @Override
    public CqlCompletion getCompletion(CqlQuery query) {

        ImmutableSortedSet.Builder<CqlPart> completionBuild = ImmutableSortedSet.naturalOrder();
        completionBuild.addAll(staticPart);

        SortedSet<CqlColumnName> columnNames = null;
        CqlTable table = extractTableName(KW_SELECT, query);
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
    public CqlPart[] startMarkers() {
        return startMarker;
    }

}

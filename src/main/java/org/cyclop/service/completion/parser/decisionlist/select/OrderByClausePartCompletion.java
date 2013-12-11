package org.cyclop.service.completion.parser.decisionlist.select;

import com.google.common.collect.ImmutableSortedSet;
import java.util.ArrayList;
import java.util.List;
import java.util.SortedSet;
import javax.annotation.PostConstruct;
import javax.inject.Inject;
import javax.inject.Named;
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
@Named("select.OrderByClausePartCompletion")
public class OrderByClausePartCompletion implements CqlPartCompletionStatic {

    private final CqlPart startMarker = new CqlKeyword("order by");

    @Inject
    private QueryService queryService;

    private final List<CqlPart> staticPart = new ArrayList<>();

    @PostConstruct
    public void init() {
        staticPart.add(new CqlKeyword("asc"));
        staticPart.add(new CqlKeyword("desc"));
        staticPart.add(new CqlKeyword("limit"));
        staticPart.add(new CqlKeyword("allow filtering"));
        staticPart.add(new CqlKeyword("token"));
    }

    @Override
    public CqlCompletion getCompletion(CqlQuery query) {
        ImmutableSortedSet.Builder<CqlPart> completionBuild = ImmutableSortedSet.naturalOrder();

        CqlTable table = extractTableName(KW_SELECT, query);
        SortedSet<CqlColumnName> columnNames = queryService.findColumnNames(table);
        completionBuild.addAll(columnNames);

        completionBuild.addAll(staticPart);

        ImmutableSortedSet<CqlPart> completion = completionBuild.build();
        CqlCompletion cmp = new CqlCompletion(completion, completion);
        return cmp;
    }

    @Override
    public CqlPart[] startMarkers() {
        return new CqlPart[]{startMarker};
    }

}


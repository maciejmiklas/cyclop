package org.cyclop.service.completion.parser.delete;

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
import static org.cyclop.model.CqlKeywords.FROM;

/**
 * @author Maciej Miklas
 */
@Named("delete.DeleteClauseCompletion")
public class DeleteClauseCompletion implements CqlPartCompletionStatic {

    @Inject
    private QueryService queryService;

    private final CqlPart[] startMarker = new CqlPart[]{new CqlKeyword("delete")};

    private final List<CqlPart> staticPart = new ArrayList<>();

    @PostConstruct
    public void init() {
        staticPart.add(new CqlKeyword("from"));
    }

    @Override
    public CqlCompletion getCompletion(CqlQuery query) {

        ImmutableSortedSet.Builder<CqlPart> completionBuild = ImmutableSortedSet.naturalOrder();
        completionBuild.addAll(staticPart);

        SortedSet<CqlColumnName> columnNames;
        CqlTable table = extractTableName(FROM, query);
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

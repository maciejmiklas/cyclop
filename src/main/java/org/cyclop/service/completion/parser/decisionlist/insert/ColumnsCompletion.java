package org.cyclop.service.completion.parser.decisionlist.insert;

import com.google.common.collect.ImmutableSortedSet;
import org.cyclop.model.*;
import org.cyclop.service.cassandra.QueryService;
import org.cyclop.service.completion.parser.decisionlist.CqlPartCompletionStatic;
import org.cyclop.service.completion.parser.decisionlist.util.DecisionHelper;

import javax.inject.Inject;
import javax.inject.Named;

import static org.cyclop.common.QueryHelper.extractTableName;
import static org.cyclop.model.CqlKeywords.FROM;
import static org.cyclop.model.CqlKeywords.INSERT;

/**
 * @author Maciej Miklas
 */
@Named("insert.ColumnsCompletion")
public class ColumnsCompletion implements CqlPartCompletionStatic {

    private final CqlPart[] startMarker = new CqlPart[]{new CqlPart("(")};

    @Inject
    private DecisionHelper decisionHelper;

    @Inject
    private QueryService queryService;

    @Override
    public CqlCompletion getCompletion(CqlQuery query) {
        CqlTable table = extractTableName(INSERT, query);
        ImmutableSortedSet<CqlColumnName> columnNames = queryService.findColumnNames(table);

        ImmutableSortedSet.Builder<CqlColumnName> fullCmp = ImmutableSortedSet.naturalOrder();
        fullCmp.addAll(decisionHelper.prependToCqlColumnName(columnNames, "("));
        fullCmp.addAll(decisionHelper.prependToCqlColumnName(columnNames, ","));
        fullCmp.addAll(columnNames);

        CqlCompletion cmp = new CqlCompletion(fullCmp.build(), columnNames);
        return cmp;
    }

    @Override
    public CqlPart[] startMarkers() {
        return startMarker;
    }

}

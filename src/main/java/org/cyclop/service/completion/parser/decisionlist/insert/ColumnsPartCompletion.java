package org.cyclop.service.completion.parser.decisionlist.insert;

import com.google.common.collect.ImmutableSortedSet;
import javax.inject.Inject;
import javax.inject.Named;
import org.cyclop.service.cassandra.QueryService;
import org.cyclop.service.completion.parser.decisionlist.CqlPartCompletionStatic;
import org.cyclop.service.completion.parser.decisionlist.util.DecisionHelper;
import org.cyclop.service.model.CqlColumnName;
import org.cyclop.service.model.CqlCompletion;
import org.cyclop.service.model.CqlPart;
import org.cyclop.service.model.CqlQuery;
import org.cyclop.service.model.CqlTable;

import static org.cyclop.common.QueryHelper.KW_INSERT;
import static org.cyclop.common.QueryHelper.extractTableName;

/**
 * @author Maciej Miklas
 */
@Named("insert.ColumnsPartCompletion")
public class ColumnsPartCompletion implements CqlPartCompletionStatic {

    private final CqlPart[] startMarker = new CqlPart[]{new CqlPart("(")};

    @Inject
    private DecisionHelper decisionHelper;

    @Inject
    private QueryService queryService;

    @Override
    public CqlCompletion getCompletion(CqlQuery query) {
        CqlTable table = extractTableName(KW_INSERT, query);
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

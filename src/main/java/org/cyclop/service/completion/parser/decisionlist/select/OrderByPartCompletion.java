package org.cyclop.service.completion.parser.decisionlist.select;

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

import static org.cyclop.common.QueryHelper.KW_SELECT;
import static org.cyclop.common.QueryHelper.extractTableName;

/**
 * @author Maciej Miklas
 */
@Named("select.OrderByPartCompletion")
public class OrderByPartCompletion implements CqlPartCompletionStatic {

    private final CqlPart[] startMarker = new CqlPart[]{new CqlPart("(")};

    @Inject
    private DecisionHelper decisionHelper;

    @Inject
    private QueryService queryService;

    @Override
    public CqlCompletion getCompletion(CqlQuery query) {

        CqlTable table = extractTableName(KW_SELECT, query);
        ImmutableSortedSet<CqlColumnName> columnNames = queryService.findColumnNames(table);

        ImmutableSortedSet.Builder<CqlPart> fullCmpBuild = ImmutableSortedSet.naturalOrder();
        fullCmpBuild.addAll(decisionHelper.prependToCqlColumnName(columnNames, "("));
        fullCmpBuild.addAll(decisionHelper.prependToCqlColumnName(columnNames, ","));
        fullCmpBuild.addAll(columnNames);

        CqlCompletion cqlCompletion = new CqlCompletion(fullCmpBuild.build(), columnNames);
        return cqlCompletion;
    }

    @Override
    public CqlPart[] startMarkers() {
        return startMarker;
    }

}

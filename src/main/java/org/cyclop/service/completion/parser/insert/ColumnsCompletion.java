package org.cyclop.service.completion.parser.insert;

import com.google.common.collect.ImmutableSortedSet;
import org.cyclop.model.*;
import org.cyclop.service.cassandra.QueryService;
import org.cyclop.service.completion.parser.CqlPartCompletionStatic;
import org.cyclop.service.completion.parser.DecisionHelper;

import javax.inject.Inject;
import javax.inject.Named;

import static org.cyclop.common.QueryHelper.extractTableName;
import static org.cyclop.model.CqlKeywords.INSERT;

/**
 * @author Maciej Miklas
 */
@Named("insert.ColumnsCompletion")
public class ColumnsCompletion implements CqlPartCompletionStatic {

    private final static CqlPart SM = new CqlPart("(");

    @Inject
    private DecisionHelper decisionHelper;

    @Inject
    private QueryService queryService;

    @Override
    public CqlCompletion getCompletion(CqlQuery query) {

        CqlTable table = extractTableName(INSERT, query);
        ImmutableSortedSet<CqlColumnName> columnNames = queryService.findColumnNames(table);

        CqlCompletion cmp = CqlCompletion.Builder.naturalOrder().full(decisionHelper.prependToCqlColumnName(columnNames, "(")).
                full(decisionHelper.prependToCqlColumnName(columnNames, ",")).full(columnNames).min(columnNames).build();
        return cmp;
    }

    @Override
    public CqlPart startMarker() {
        return SM;
    }

}

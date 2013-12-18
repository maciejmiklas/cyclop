package org.cyclop.service.completion.parser.update;

import org.cyclop.model.*;
import org.cyclop.service.cassandra.QueryService;
import org.cyclop.service.completion.parser.CqlPartCompletionStatic;

import javax.inject.Inject;
import javax.inject.Named;
import java.util.SortedSet;

import static org.cyclop.common.QueryHelper.extractTableName;
import static org.cyclop.model.CqlKeywords.UPDATE;

/**
 * @author Maciej Miklas
 */
@Named("update.WhereCompletion")
public class WhereCompletion implements CqlPartCompletionStatic {

    private final CqlPart SM = new CqlKeyword("where");

    @Inject
    private QueryService queryService;


    @Override
    public CqlCompletion getCompletion(CqlQuery query) {
        CqlCompletion.Builder cb = CqlCompletion.Builder.naturalOrder();
        cb.all(new CqlKeyword("and"));

        CqlTable table = extractTableName(UPDATE, query);
        SortedSet<CqlColumnName> columnNames = queryService.findColumnNames(table);

        cb.all(columnNames);
        CqlCompletion cmp = cb.build();
        return cmp;
    }

    @Override
    public CqlPart startMarker() {
        return SM;
    }

}

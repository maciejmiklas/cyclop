package org.cyclop.service.completion.parser.delete;

import org.cyclop.model.*;
import org.cyclop.service.cassandra.QueryService;
import org.cyclop.service.completion.parser.CqlPartCompletionStatic;

import javax.inject.Inject;
import javax.inject.Named;
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

    private final static CqlPart SM = new CqlKeyword("delete");

    @Override
    public CqlCompletion getCompletion(CqlQuery query) {

        CqlCompletion.Builder cb = CqlCompletion.Builder.naturalOrder();
        cb.all(new CqlKeyword("from"));

        SortedSet<CqlColumnName> columnNames;
        CqlTable table = extractTableName(FROM, query);
        if (queryService.checkTableExists(table)) {
            columnNames = queryService.findColumnNames(table);
        } else {
            columnNames = queryService.findAllColumnNames();
        }

        cb.all(columnNames);

        return cb.build();
    }

    @Override
    public CqlPart startMarker() {
        return SM;
    }

}

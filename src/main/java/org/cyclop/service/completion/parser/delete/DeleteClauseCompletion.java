package org.cyclop.service.completion.parser.delete;

import com.google.common.base.Objects;
import java.util.SortedSet;
import javax.inject.Inject;
import javax.inject.Named;
import org.cyclop.model.CqlColumnName;
import org.cyclop.model.CqlCompletion;
import org.cyclop.model.CqlKeyword;
import org.cyclop.model.CqlQuery;
import org.cyclop.model.CqlTable;
import org.cyclop.service.cassandra.QueryService;
import org.cyclop.service.completion.parser.MarkerBasedCompletion;

import static org.cyclop.common.QueryHelper.extractTableName;

/**
 * @author Maciej Miklas
 */
@Named("delete.DeleteClauseCompletion")
public class DeleteClauseCompletion extends MarkerBasedCompletion {

    @Inject
    private QueryService queryService;

    public DeleteClauseCompletion() {
        super(CqlKeyword.Def.DELETE.value);
    }

    @Override
    public CqlCompletion getCompletion(CqlQuery query) {

        CqlCompletion.Builder cb = CqlCompletion.Builder.naturalOrder();
        cb.all(CqlKeyword.Def.FROM.value);

        SortedSet<CqlColumnName> columnNames;
        CqlTable table = extractTableName(CqlKeyword.Def.FROM.value, query);
        if (queryService.checkTableExists(table)) {
            columnNames = queryService.findColumnNames(table);
        } else {
            columnNames = queryService.findAllColumnNames();
        }

        cb.all(columnNames);

        return cb.build();
    }

    @Override
    public String toString() {
        return Objects.toStringHelper(this).toString();
    }

}

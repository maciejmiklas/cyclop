package org.cyclop.service.completion.parser.update;

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
@Named("update.WhereCompletion")
public class WhereCompletion extends MarkerBasedCompletion {

    @Inject
    private QueryService queryService;

    public WhereCompletion() {
        super(CqlKeyword.Def.WHERE.value);
    }

    @Override
    public CqlCompletion getCompletion(CqlQuery query) {
        CqlCompletion.Builder cb = CqlCompletion.Builder.naturalOrder();
        cb.all(CqlKeyword.Def.AND.value);

        CqlTable table = extractTableName(CqlKeyword.Def.UPDATE.value, query);
        SortedSet<CqlColumnName> columnNames = queryService.findColumnNames(table);

        cb.all(columnNames);
        CqlCompletion cmp = cb.build();
        return cmp;
    }

    @Override
    public String toString() {
        return Objects.toStringHelper(this).toString();
    }

}

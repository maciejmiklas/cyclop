package org.cyclop.service.completion.impl.parser.template;

import javax.inject.Inject;
import javax.inject.Named;
import org.cyclop.common.QueryHelper;
import org.cyclop.model.CqlKeyword;
import org.cyclop.model.CqlQuery;
import org.cyclop.model.CqlTable;
import org.cyclop.service.cassandra.QueryService;
import org.cyclop.service.completion.impl.parser.OffsetBasedCompletion;

/**
 * @author Maciej Miklas
 */
@Named
public abstract class AfterTableNameCompletionTemplate implements OffsetBasedCompletion {

    @Inject
    private QueryService queryService;

    private CqlKeyword cqlKeyword;

    public AfterTableNameCompletionTemplate(CqlKeyword cqlKeyword) {
        if (cqlKeyword == null) {
            throw new IllegalArgumentException("Null cqlKeyword");
        }
        this.cqlKeyword = cqlKeyword;
    }

    @Override
    public final int canApply(CqlQuery query, int queryPosition) {
        CqlTable table = QueryHelper.extractTableName(cqlKeyword, query);
        if (table == null) {
            return -1;
        }

        int index = -1;
        if (queryService.checkTableExists(table)) {
            index = query.cqlLc.indexOf(table.partLc) + table.partLc.length();
        }
        return index;
    }
}

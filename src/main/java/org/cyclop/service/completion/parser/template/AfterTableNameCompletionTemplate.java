package org.cyclop.service.completion.parser.template;

import org.cyclop.common.QueryHelper;
import org.cyclop.model.CqlKeywords;
import org.cyclop.model.CqlQuery;
import org.cyclop.model.CqlTable;
import org.cyclop.service.cassandra.QueryService;
import org.cyclop.service.completion.parser.CqlPartCompletionDynamic;

import javax.inject.Inject;
import javax.inject.Named;

/**
 * @author Maciej Miklas
 */
@Named
public abstract class AfterTableNameCompletionTemplate implements CqlPartCompletionDynamic {

    @Inject
    private QueryService queryService;

    private CqlKeywords cqlKeywords;

    public AfterTableNameCompletionTemplate(CqlKeywords cqlKeywords) {
        this.cqlKeywords = cqlKeywords;

    }

    @Override
    public final int canApply(CqlQuery query, int queryPosition) {

        CqlTable table = QueryHelper.extractTableName(cqlKeywords, query);
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

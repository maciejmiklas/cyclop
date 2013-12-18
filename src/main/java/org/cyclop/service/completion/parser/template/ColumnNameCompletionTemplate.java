package org.cyclop.service.completion.parser.template;

import org.cyclop.model.*;
import org.cyclop.service.cassandra.CassandraSession;
import org.cyclop.service.cassandra.QueryService;
import org.cyclop.service.completion.parser.CqlPartCompletionStatic;

import javax.inject.Inject;
import javax.inject.Named;
import java.util.Collection;
import java.util.SortedSet;

import static org.cyclop.common.QueryHelper.extractTableName;

@Named
public abstract class ColumnNameCompletionTemplate implements CqlPartCompletionStatic {

    @Inject
    private QueryService queryService;

    private final CqlCompletion.BuilderTemplate builderTemplate;

    @Inject
    private CassandraSession session;

    private CqlKeywords cqlKeywords;

    public ColumnNameCompletionTemplate(Collection<? extends CqlPart> staticPartAll, CqlKeywords cqlKeywords) {
        this.builderTemplate = CqlCompletion.Builder.naturalOrder().all(staticPartAll).template();
        this.cqlKeywords = cqlKeywords;
    }


    @Override
    public final CqlCompletion getCompletion(CqlQuery query) {
        CqlCompletion.Builder builder = builderTemplate.naturalOrder();

        SortedSet<CqlColumnName> columnNames;
        CqlTable table = extractTableName(cqlKeywords, query);
        if (queryService.checkTableExists(table)) {
            columnNames = queryService.findColumnNames(table);
        } else {
            columnNames = queryService.findAllColumnNames();
        }

        builder.all(columnNames);

        CqlCompletion cmp = builder.build();
        return cmp;
    }


}
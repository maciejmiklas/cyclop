package org.cyclop.service.completion.parser.select;

import org.cyclop.model.*;
import org.cyclop.service.cassandra.CassandraSession;
import org.cyclop.service.cassandra.QueryService;
import org.cyclop.service.completion.parser.CqlPartCompletionStatic;

import javax.annotation.PostConstruct;
import javax.inject.Inject;
import javax.inject.Named;
import java.util.SortedSet;

import static org.cyclop.common.QueryHelper.extractTableName;
import static org.cyclop.model.CqlKeywords.FROM;

/**
 * @author Maciej Miklas
 */
@Named("select.WhereClausePartCompletion")
public class WhereCompletion implements CqlPartCompletionStatic {

    private final CqlPart SM = new CqlKeyword("where");

    @Inject
    private QueryService queryService;

    @Inject
    private CassandraSession session;

    private CqlCompletion.BuilderTemplate builderTemplate;

    @PostConstruct
    public void init() {
        builderTemplate = CqlCompletion.Builder.naturalOrder().all(new CqlKeyword("limit")).all(new CqlKeyword("allow filtering")).
                all(new CqlKeyword("and")).full(new CqlKeyword("in (")).min(new CqlKeyword("in")).
                full(new CqlKeyword("order by (")).min(new CqlKeyword("order by")).template();
    }

    @Override
    public CqlCompletion getCompletion(CqlQuery query) {
        CqlCompletion.Builder builder = builderTemplate.naturalOrder();

        CqlTable table = extractTableName(FROM, query);
        SortedSet<CqlColumnName> columnNames = queryService.findColumnNames(table);
        builder.all(columnNames);

        CqlCompletion cmp = builder.build();
        return cmp;
    }

    @Override
    public CqlPart startMarker() {
        return SM;
    }

}


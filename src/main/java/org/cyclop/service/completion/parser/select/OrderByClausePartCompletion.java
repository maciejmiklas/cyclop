package org.cyclop.service.completion.parser.select;

import com.google.common.collect.ImmutableSortedSet;
import org.cyclop.model.*;
import org.cyclop.service.cassandra.QueryService;
import org.cyclop.service.completion.parser.CqlPartCompletionStatic;

import javax.annotation.PostConstruct;
import javax.inject.Inject;
import javax.inject.Named;
import java.util.ArrayList;
import java.util.List;
import java.util.SortedSet;

import static org.cyclop.common.QueryHelper.extractTableName;
import static org.cyclop.model.CqlKeywords.FROM;

/**
 * @author Maciej Miklas
 */
@Named("select.OrderByClausePartCompletion")
public class OrderByClausePartCompletion implements CqlPartCompletionStatic {

    private final static CqlPart SM = new CqlKeyword("order by");

    @Inject
    private QueryService queryService;

    private CqlCompletion.BuilderTemplate builderTemplate;

    @PostConstruct
    public void init() {
        builderTemplate = CqlCompletion.Builder.naturalOrder().all(new CqlKeyword("asc")).all(new CqlKeyword("desc")).
                all(new CqlKeyword("limit")).all(new CqlKeyword("allow filtering")).all(new CqlKeyword("token")).template();
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


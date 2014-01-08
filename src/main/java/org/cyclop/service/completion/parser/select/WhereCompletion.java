package org.cyclop.service.completion.parser.select;

import java.util.SortedSet;
import javax.annotation.PostConstruct;
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
@Named("select.WhereClausePartCompletion")
public class WhereCompletion extends MarkerBasedCompletion {

    @Inject
    private QueryService queryService;

    private CqlCompletion.BuilderTemplate builderTemplate;

    public WhereCompletion() {
        super(CqlKeyword.Def.WHERE.value);
    }

    @PostConstruct
    public void init() {
        builderTemplate = CqlCompletion.Builder.naturalOrder().all(CqlKeyword.Def.LIMIT.value).all(CqlKeyword.Def
                .ALLOW_FILTERING.value).all(CqlKeyword.Def.AND.value).full(CqlKeyword.Def.IN_BL.value).min(CqlKeyword
                .Def.IN.value).all(CqlKeyword.Def.ORDER_BY.value).template();
    }

    @Override
    public CqlCompletion getCompletion(CqlQuery query) {
        CqlCompletion.Builder builder = builderTemplate.naturalOrder();

        CqlTable table = extractTableName(CqlKeyword.Def.FROM.value, query);
        SortedSet<CqlColumnName> columnNames = queryService.findColumnNames(table);
        builder.all(columnNames);

        CqlCompletion cmp = builder.build();
        return cmp;
    }

}

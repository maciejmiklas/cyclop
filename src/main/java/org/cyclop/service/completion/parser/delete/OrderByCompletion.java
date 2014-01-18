package org.cyclop.service.completion.parser.delete;

import com.google.common.base.Objects;
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
@Named("delete.OrderByCompletion")
public class OrderByCompletion extends MarkerBasedCompletion {

    @Inject
    private QueryService queryService;

    private CqlCompletion.BuilderTemplate builderTemplate;

    public OrderByCompletion() {
        super(CqlKeyword.Def.ORDER_BY.value);
    }

    @PostConstruct
    public void init() {
        builderTemplate = CqlCompletion.Builder.naturalOrder().all(CqlKeyword.Def.ASC.value).all(CqlKeyword.Def.DESC.value)
                .all(CqlKeyword.Def.LIMIT.value).all(CqlKeyword.Def.ALLOW_FILTERING.value).all(CqlKeyword.Def.TOKEN.value)
                .template();
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

    @Override
    public String toString() {
        return Objects.toStringHelper(this).toString();
    }

}

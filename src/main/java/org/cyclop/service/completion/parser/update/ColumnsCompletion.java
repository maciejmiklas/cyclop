package org.cyclop.service.completion.parser.update;

import com.google.common.collect.ImmutableSortedSet;
import org.cyclop.model.*;
import org.cyclop.service.cassandra.QueryService;
import org.cyclop.service.completion.parser.CqlPartCompletionStatic;

import javax.annotation.PostConstruct;
import javax.inject.Inject;
import javax.inject.Named;

import static org.cyclop.common.QueryHelper.extractTableName;
import static org.cyclop.model.CqlKeywords.UPDATE;

/**
 * @author Maciej Miklas
 */
@Named("update.ColumnsCompletion")
public class ColumnsCompletion implements CqlPartCompletionStatic {

    private final static CqlPart SM = new CqlPart("set");

    private CqlCompletion.BuilderTemplate builderTemplate;

    @Inject
    private QueryService queryService;

    @PostConstruct
    public void init() {
        builderTemplate = CqlCompletion.Builder.naturalOrder().all(new CqlKeyword("where")).template();
    }

    @Override
    public CqlCompletion getCompletion(CqlQuery query) {
        CqlCompletion.Builder builder = builderTemplate.naturalOrder();

        CqlTable table = extractTableName(UPDATE, query);
        ImmutableSortedSet<CqlColumnName> columnNames = queryService.findColumnNames(table);
        builder.all(columnNames);

        CqlCompletion cmp = builder.build();
        return cmp;
    }

    @Override
    public CqlPart startMarker() {
        return SM;
    }

}

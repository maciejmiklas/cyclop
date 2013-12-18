package org.cyclop.service.completion.parser.delete;

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
@Named("delete.WhereClauseCompletion")
public class WhereClauseCompletion implements CqlPartCompletionStatic {

    private final static CqlPart SM = new CqlKeyword("where");

    @Inject
    private QueryService queryService;

    private CqlCompletion.BuilderTemplate builderTemplate;

    @PostConstruct
    public void init() {
        builderTemplate =  CqlCompletion.Builder.naturalOrder().all(new CqlKeyword("and")).full(new CqlKeyword("in ("))
                .min(new CqlKeyword("in")).template();
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

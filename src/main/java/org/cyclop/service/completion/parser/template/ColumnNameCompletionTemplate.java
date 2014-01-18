package org.cyclop.service.completion.parser.template;

import java.util.Collection;
import java.util.SortedSet;
import javax.inject.Inject;
import javax.inject.Named;
import org.cyclop.model.CqlColumnName;
import org.cyclop.model.CqlCompletion;
import org.cyclop.model.CqlKeyword;
import org.cyclop.model.CqlPart;
import org.cyclop.model.CqlQuery;
import org.cyclop.model.CqlTable;
import org.cyclop.service.cassandra.QueryService;
import org.cyclop.service.completion.parser.MarkerBasedCompletion;

import static org.cyclop.common.QueryHelper.extractTableName;

@Named
public abstract class ColumnNameCompletionTemplate extends MarkerBasedCompletion {

    @Inject
    private QueryService queryService;

    private final CqlCompletion.BuilderTemplate builderTemplate;

    private CqlKeyword cqlKeyword;

    // TODO use builder instead of collection
    public ColumnNameCompletionTemplate(Collection<? extends CqlPart> staticPartAll, CqlKeyword cqlKeyword,
                                        CqlPart startMarker) {
        super(startMarker);
        this.builderTemplate = CqlCompletion.Builder.naturalOrder().all(staticPartAll).template();
        this.cqlKeyword = cqlKeyword;
    }

    @Override
    public final CqlCompletion getCompletion(CqlQuery query) {
        CqlCompletion.Builder builder = builderTemplate.naturalOrder();

        SortedSet<CqlColumnName> columnNames;
        CqlTable table = extractTableName(cqlKeyword, query);
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

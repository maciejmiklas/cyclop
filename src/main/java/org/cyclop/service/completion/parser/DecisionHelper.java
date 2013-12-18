package org.cyclop.service.completion.parser;

import com.datastax.driver.core.DataType;
import com.google.common.collect.ImmutableSortedSet;
import org.cyclop.common.QueryHelper;
import org.cyclop.model.*;
import org.cyclop.service.cassandra.QueryService;

import javax.inject.Inject;
import javax.inject.Named;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

/**
 * @author Maciej Miklas
 */
@Named
public final class DecisionHelper {

    @Inject
    private QueryService queryService;

    public static List<CqlPart> prependToCqlPart(Collection<CqlPart> col, String value) {
        List<CqlPart> newElements = new ArrayList<>(col.size());
        for (CqlPart part : col) {
            newElements.add(new CqlPart(value + part.part));
        }
        return newElements;
    }

    public List<CqlColumnName> prependToCqlColumnName(Collection<CqlColumnName> col, String value) {
        List<CqlColumnName> newElements = new ArrayList<>(col.size());
        for (CqlColumnName part : col) {
            newElements.add(new CqlColumnName(DataType.text(), value + part.part));
        }
        return newElements;
    }

    public CqlCompletion computeTableNameCompletion(CqlKeywords kw, CqlQuery query) {
        CqlCompletion completion = computeTableNameCompletionWithKeyspaceInQuery(kw, query);
        if (completion == null) {
            completion = computeTableNameCompletionWithoutKeyspaceInQuery();
        }
        return completion;
    }

    private CqlCompletion computeTableNameCompletionWithKeyspaceInQuery(CqlKeywords kw, CqlQuery query) {
        CqlKeySpace keySpace = QueryHelper.extractKeyspace(kw, query);
        if (keySpace == null) {
            return null;
        }
        ImmutableSortedSet<CqlTable> tables = queryService.findTableNames(keySpace);
        if (tables.isEmpty()) {
            return null;
        }

        CqlCompletion.Builder builder = CqlCompletion.Builder.naturalOrder();
        builder.all(tables);

        for (CqlTable ta : tables) {
            builder.full(new CqlKeySpace(keySpace.partLc + "." + ta.partLc));
        }

        CqlCompletion cmp = builder.build();
        return cmp;
    }

    private CqlCompletion computeTableNameCompletionWithoutKeyspaceInQuery() {
        CqlCompletion.Builder builder = CqlCompletion.Builder.naturalOrder();

        ImmutableSortedSet<CqlTable> tables = queryService.findTableNamesForActiveKeySpace();
        builder.all(tables);

        ImmutableSortedSet<CqlKeySpace> keyspaces = queryService.findAllKeySpaces();
        for (CqlKeySpace ks : keyspaces) {
            builder.min(ks);
            builder.full(new CqlKeySpace(ks.partLc + "."));
        }

        CqlCompletion cmp = builder.build();
        return cmp;
    }

}

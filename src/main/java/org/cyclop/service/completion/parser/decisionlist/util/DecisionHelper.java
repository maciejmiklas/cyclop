package org.cyclop.service.completion.parser.decisionlist.util;

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

        ImmutableSortedSet.Builder<CqlPart> minCompletionBuild = ImmutableSortedSet.naturalOrder();
        ImmutableSortedSet.Builder<CqlPart> fullCompletionBuild = ImmutableSortedSet.naturalOrder();

        minCompletionBuild.addAll(tables);
        fullCompletionBuild.addAll(tables);

        for (CqlTable ta : tables) {
            fullCompletionBuild.add(new CqlKeySpace(keySpace.partLc + "." + ta.partLc));
        }

        CqlCompletion cmp = new CqlCompletion(fullCompletionBuild.build(), minCompletionBuild.build());
        return cmp;
    }

    private CqlCompletion computeTableNameCompletionWithoutKeyspaceInQuery() {
        ImmutableSortedSet.Builder<CqlPart> minCompletionBuild = ImmutableSortedSet.naturalOrder();
        ImmutableSortedSet.Builder<CqlPart> fullCompletionBuild = ImmutableSortedSet.naturalOrder();

        ImmutableSortedSet<CqlTable> tables = queryService.findTableNamesForActiveKeySpace();
        minCompletionBuild.addAll(tables);
        fullCompletionBuild.addAll(tables);

        ImmutableSortedSet<CqlKeySpace> keyspaces = queryService.findAllKeySpaces();
        for (CqlKeySpace ks : keyspaces) {
            minCompletionBuild.add(ks);
            fullCompletionBuild.add(new CqlKeySpace(ks.partLc + "."));
        }

        CqlCompletion cmp = new CqlCompletion(fullCompletionBuild.build(), minCompletionBuild.build());
        return cmp;
    }

}

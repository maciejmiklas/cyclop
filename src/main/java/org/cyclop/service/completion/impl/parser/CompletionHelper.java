package org.cyclop.service.completion.impl.parser;

import com.google.common.collect.ImmutableSortedSet;
import org.cyclop.common.QueryHelper;
import org.cyclop.model.CqlCompletion;
import org.cyclop.model.CqlKeySpace;
import org.cyclop.model.CqlKeyword;
import org.cyclop.model.CqlQuery;
import org.cyclop.model.CqlTable;
import org.cyclop.service.cassandra.QueryScope;
import org.cyclop.service.cassandra.QueryService;

import javax.inject.Inject;
import javax.inject.Named;

/** @author Maciej Miklas */
@Named
public final class CompletionHelper {

	@Inject
	private QueryService queryService;

	@Inject
	private QueryScope queryScope;

	public CqlCompletion.Builder computeTableNameCompletion(CqlQuery query, CqlKeyword... kw) {
		CqlCompletion.Builder completion = computeTableNameCompletionWithKeyspaceInQuery(query, kw);
		if (completion == null) {
			completion = computeTableNameCompletionWithoutKeyspaceInQuery();
		}
		return completion;
	}

	private CqlCompletion.Builder computeTableNameCompletionWithKeyspaceInQuery(CqlQuery query, CqlKeyword... kw) {
		CqlKeySpace keySpace = QueryHelper.extractKeyspace(query, kw);
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
			builder.full(new CqlTable(keySpace.part, ta.part));
		}

		return builder;
	}

	private CqlCompletion.Builder computeTableNameCompletionWithoutKeyspaceInQuery() {
		CqlCompletion.Builder builder = CqlCompletion.Builder.naturalOrder();

		CqlKeySpace activeKeySpace = queryScope.getActiveKeySpace();
		ImmutableSortedSet<CqlTable> tables = queryService.findTableNames(activeKeySpace);
		builder.all(tables);

		ImmutableSortedSet<CqlKeySpace> keyspaces = queryService.findAllKeySpaces();
		for (CqlKeySpace ks : keyspaces) {
			builder.min(ks);
			builder.full(new CqlKeySpace(ks.partLc + "."));
		}

		return builder;
	}

}

package org.cyclop.service.completion.impl.parser.insert;

import com.google.common.base.Objects;
import com.google.common.collect.ImmutableSortedSet;
import org.cyclop.model.CqlColumnName;
import org.cyclop.model.CqlCompletion;
import org.cyclop.model.CqlKeyword;
import org.cyclop.model.CqlPart;
import org.cyclop.model.CqlQuery;
import org.cyclop.model.CqlTable;
import org.cyclop.service.cassandra.QueryService;
import org.cyclop.service.completion.impl.parser.MarkerBasedCompletion;

import javax.inject.Inject;
import javax.inject.Named;

import static org.cyclop.common.QueryHelper.extractTableName;

/** @author Maciej Miklas */
@Named("insert.ColumnsCompletion") class ColumnsCompletion extends MarkerBasedCompletion {

	@Inject
	private QueryService queryService;

	public ColumnsCompletion() {
		super(new CqlPart("("));
	}

	@Override
	public CqlCompletion getCompletion(CqlQuery query) {

		CqlTable table = extractTableName(CqlKeyword.Def.INSERT_INTO.value, query);
		ImmutableSortedSet<CqlColumnName> columnNames = queryService.findColumnNames(table);

		CqlCompletion cmp = CqlCompletion.Builder.naturalOrder().all(columnNames).build();
		return cmp;
	}

	@Override
	public String toString() {
		return Objects.toStringHelper(this).toString();
	}

}

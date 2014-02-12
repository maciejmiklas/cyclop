package org.cyclop.service.completion.impl.parser.update;

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

import javax.annotation.PostConstruct;
import javax.inject.Inject;
import javax.inject.Named;

import static org.cyclop.common.QueryHelper.extractTableName;

/** @author Maciej Miklas */
@Named("update.ColumnsCompletion") class ColumnsCompletion extends MarkerBasedCompletion {

	private CqlCompletion.BuilderTemplate builderTemplate;

	@Inject
	private QueryService queryService;

	public ColumnsCompletion() {
		super(new CqlPart("set"));
	}

	@PostConstruct
	public void init() {
		builderTemplate = CqlCompletion.Builder.naturalOrder().all(CqlKeyword.Def.WHERE.value).template();
	}

	@Override
	public CqlCompletion getCompletion(CqlQuery query) {
		CqlCompletion.Builder builder = builderTemplate.naturalOrder();

		CqlTable table = extractTableName(CqlKeyword.Def.UPDATE.value, query);
		ImmutableSortedSet<CqlColumnName> columnNames = queryService.findColumnNames(table);
		builder.all(columnNames);

		CqlCompletion cmp = builder.build();
		return cmp;
	}

	@Override
	public String toString() {
		return Objects.toStringHelper(this).toString();
	}

}

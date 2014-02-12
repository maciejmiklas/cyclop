package org.cyclop.service.completion.impl.parser.delete;

import com.google.common.base.Objects;
import org.cyclop.model.CqlColumnName;
import org.cyclop.model.CqlCompletion;
import org.cyclop.model.CqlKeyword;
import org.cyclop.model.CqlQuery;
import org.cyclop.model.CqlTable;
import org.cyclop.service.cassandra.QueryService;
import org.cyclop.service.completion.impl.parser.MarkerBasedCompletion;

import javax.annotation.PostConstruct;
import javax.inject.Inject;
import javax.inject.Named;
import java.util.SortedSet;

import static org.cyclop.common.QueryHelper.extractTableName;

/** @author Maciej Miklas */
@Named("delete.WhereClauseCompletion") class WhereClauseCompletion extends MarkerBasedCompletion {

	@Inject
	private QueryService queryService;

	private CqlCompletion.BuilderTemplate builderTemplate;

	public WhereClauseCompletion() {
		super(CqlKeyword.Def.WHERE.value);
	}

	@PostConstruct
	public void init() {
		builderTemplate = CqlCompletion.Builder.naturalOrder().all(CqlKeyword.Def.AND.value).full(CqlKeyword.Def.IN_BL.value)
				.min(CqlKeyword.Def.IN.value).template();
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

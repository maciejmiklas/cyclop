package org.cyclop.service.completion.impl.parser.select;

import com.google.common.base.Objects;
import org.cyclop.model.CqlCompletion;
import org.cyclop.model.CqlKeyword;
import org.cyclop.service.completion.impl.parser.template.ColumnNameCompletionTemplate;

import javax.inject.Named;

/** @author Maciej Miklas */
@Named("select.SelectCompletionTemplate") class SelectCompletionTemplate extends ColumnNameCompletionTemplate {

	private final static CqlCompletion.Builder STATC_PART = CqlCompletion.Builder.naturalOrder()
			.all(CqlKeyword.Def.COUNT_AST.value).all(CqlKeyword.Def.COUNT_ONE.value)
			.all(CqlKeyword.Def.WRITETIME.value).all(CqlKeyword.Def.TTL.value).all(CqlKeyword.Def.FROM.value);

	public SelectCompletionTemplate() {
		super(STATC_PART, CqlKeyword.Def.FROM.value, CqlKeyword.Def.SELECT.value);
	}

	@Override
	public String toString() {
		return Objects.toStringHelper(this).toString();
	}

}

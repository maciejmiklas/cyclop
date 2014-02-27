package org.cyclop.service.completion.impl.parser.select;

import com.google.common.base.Objects;
import org.cyclop.model.CqlCompletion;
import org.cyclop.model.CqlKeyword;
import org.cyclop.model.CqlQuery;
import org.cyclop.service.completion.impl.parser.template.AfterTableNameCompletionTemplate;

import javax.annotation.PostConstruct;
import javax.inject.Named;

/** @author Maciej Miklas */
@Named("select.AfterFromCompletion")
class AfterFromCompletion extends AfterTableNameCompletionTemplate {

	private CqlCompletion completion;

	public AfterFromCompletion() {
		super(CqlKeyword.Def.FROM.value);
	}

	@PostConstruct
	public void init() {
		completion = CqlCompletion.Builder.naturalOrder().all(CqlKeyword.Def.WHERE.value)
				.all(CqlKeyword.Def.ORDER_BY.value).all(CqlKeyword.Def.LIMIT.value)
				.all(CqlKeyword.Def.ALLOW_FILTERING.value).build();
	}

	@Override
	public CqlCompletion getCompletion(CqlQuery query) {
		return completion;
	}

	@Override
	public String toString() {
		return Objects.toStringHelper(this).add("completion", completion).toString();
	}
}

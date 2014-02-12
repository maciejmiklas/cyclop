package org.cyclop.service.completion.impl.parser.createkeyspace;

import com.google.common.base.Objects;
import org.cyclop.model.CqlCompletion;
import org.cyclop.model.CqlKeyword;
import org.cyclop.model.CqlKeywordValue;
import org.cyclop.service.completion.impl.parser.template.StaticMarkerBasedCompletion;

import javax.inject.Named;

/** @author Maciej Miklas */
@Named("createkeyspace.WithCompletion") class WithCompletion extends StaticMarkerBasedCompletion {

	public WithCompletion() {
		super(CqlKeyword.Def.WITH.value, CqlCompletion.Builder.naturalOrder()
				.all(CqlKeyword.Def.VALUES.value).all(CqlKeyword.Def.AND.value)
				.all(CqlKeyword.Def.REPLICATION.value).all(CqlKeywordValue.Def.DURABLE_WRITES.value)
				.prefix(CqlKeywordValue.Def.CLASS.value).prefix(CqlKeywordValue.Def.SIMPLE_STRATEGY.value)
				.prefix(CqlKeywordValue.Def.REPLICATION_FACTOR.value)
				.prefix(CqlKeywordValue.Def.NETWORK_TOPOLOGY_STRATEGY.value)
				.prefix(CqlKeywordValue.Def.DURABLE_WRITES.value).all(CqlKeywordValue.Def.TRUE.value)
				.all(CqlKeywordValue.Def.FALSE.value)
				.prefix(CqlKeywordValue.Def.OLD_NETWORK_TOPOLOGY_STRATEGY.value).build());
	}

	@Override
	public String toString() {
		return Objects.toStringHelper(this).toString();
	}

}

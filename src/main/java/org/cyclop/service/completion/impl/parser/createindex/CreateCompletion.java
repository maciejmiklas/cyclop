package org.cyclop.service.completion.impl.parser.createindex;

import com.google.common.base.Objects;
import org.cyclop.model.CqlNotSupported;
import org.cyclop.service.completion.impl.parser.NotSupportedCompletion;

import javax.inject.Named;

/** @author Maciej Miklas */
@Named("createindex.CreateCompletion") class CreateCompletion extends NotSupportedCompletion {

	public CreateCompletion() {
		super(new CqlNotSupported("create"));
	}

	@Override
	protected String getNotSupportedText() {
		return "create index";
	}

	@Override
	public String toString() {
		return Objects.toStringHelper(this).toString();
	}
}

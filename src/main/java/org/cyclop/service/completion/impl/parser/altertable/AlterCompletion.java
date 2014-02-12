package org.cyclop.service.completion.impl.parser.altertable;

import com.google.common.base.Objects;
import org.cyclop.model.CqlNotSupported;
import org.cyclop.service.completion.impl.parser.NotSupportedCompletion;

import javax.inject.Named;

/** @author Maciej Miklas */
@Named("altertable.AlterCompletion") class AlterCompletion extends NotSupportedCompletion {

	public AlterCompletion() {
		super(new CqlNotSupported("alter"));
	}

	@Override
	protected String getNotSupportedText() {
		return "alter table";
	}

	@Override
	public String toString() {
		return Objects.toStringHelper(this).toString();
	}
}

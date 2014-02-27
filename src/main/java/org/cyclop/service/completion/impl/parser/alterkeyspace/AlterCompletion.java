package org.cyclop.service.completion.impl.parser.alterkeyspace;

import com.google.common.base.Objects;
import org.cyclop.model.CqlNotSupported;
import org.cyclop.service.completion.impl.parser.NotSupportedCompletion;

import javax.inject.Named;

/** @author Maciej Miklas */
@Named("alterkeyspace.AlterCompletion")
class AlterCompletion extends NotSupportedCompletion {

	public AlterCompletion() {
		super(new CqlNotSupported("alter"));
	}

	@Override
	protected String getNotSupportedText() {
		return "alter keyspace";
	}

	@Override
	public String toString() {
		return Objects.toStringHelper(this).toString();
	}
}

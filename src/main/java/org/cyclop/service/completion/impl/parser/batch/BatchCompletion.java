package org.cyclop.service.completion.impl.parser.batch;

import com.google.common.base.Objects;
import org.cyclop.model.CqlNotSupported;
import org.cyclop.service.completion.impl.parser.NotSupportedCompletion;

import javax.inject.Named;

/** @author Maciej Miklas */
@Named("batch.BatchCompletion") class BatchCompletion extends NotSupportedCompletion {

	public BatchCompletion() {
		super(new CqlNotSupported("batch"));
	}

	@Override
	protected String getNotSupportedText() {
		return "batch";
	}

	@Override
	public String toString() {
		return Objects.toStringHelper(this).toString();
	}
}

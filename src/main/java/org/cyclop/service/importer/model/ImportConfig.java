package org.cyclop.service.importer.model;

import com.google.common.base.Objects;

/** @author Maciej Miklas */
public class ImportConfig {

	private boolean updateHistory = false;

	private boolean continueWithErrors = false;

	public ImportConfig withUpdateHistory(boolean updateHistory) {
		this.updateHistory = updateHistory;
		return this;
	}

	public ImportConfig withContinueWithErrors(boolean continueWithErrors) {
		this.continueWithErrors = continueWithErrors;
		return this;
	}

	public boolean isUpdateHistory() {
		return updateHistory;
	}

	public boolean isContinueWithErrors() {
		return continueWithErrors;
	}

	@Override
	public String toString() {
		return Objects.toStringHelper(this).add("updateHistory", updateHistory)
				.add("continueWithErrors", continueWithErrors).toString();
	}
}

package org.cyclop.web.panels.queryimport;

import com.google.common.base.Objects;

import java.io.Serializable;

/** @author Maciej Miklas */
public final class ImportOptions implements Serializable {
	private boolean includeInHistory = true;

	private boolean continueWithErrors = false;

	private boolean parallel = false;

	private String scriptFile;

	@Override
	public String toString() {
		return Objects.toStringHelper(this).add("includeInHistory", includeInHistory)
				.add("continueWithErrors", continueWithErrors).add("parallel", parallel).add("scriptFile", scriptFile)
				.toString();
	}

	public boolean isParallel() {
		return parallel;
	}

	public void setParallel(boolean parallel) {
		this.parallel = parallel;
	}

	public boolean isIncludeInHistory() {
		return includeInHistory;
	}

	public void setIncludeInHistory(boolean includeInHistory) {
		this.includeInHistory = includeInHistory;
	}

	public boolean isContinueWithErrors() {
		return continueWithErrors;
	}

	public void setContinueWithErrors(boolean continueWithErrors) {
		this.continueWithErrors = continueWithErrors;
	}

	public String getScriptFile() {
		return scriptFile;
	}

	public void setScriptFile(String scriptFile) {
		this.scriptFile = scriptFile;
	}

}

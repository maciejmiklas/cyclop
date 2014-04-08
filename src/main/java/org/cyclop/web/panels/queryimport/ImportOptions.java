package org.cyclop.web.panels.queryimport;

import java.io.Serializable;

/** @author Maciej Miklas */
public final class ImportOptions implements Serializable {
	private boolean includeInHistory = true;

	private boolean continueWithErrors = false;

	private String scriptFile;

	@Override
	public String toString() {
		return "ImportOptions [includeInHistory=" + includeInHistory + ", continueWithErrors=" + continueWithErrors +
				", scriptFile=" + scriptFile + "]";
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

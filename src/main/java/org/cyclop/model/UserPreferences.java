package org.cyclop.model;

import com.google.common.base.Objects;
import org.cyclop.model.adapter.BooleanDefaultTrueAdapter;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;
import java.io.Serializable;

/** @author Maciej Miklas */
@XmlRootElement
@XmlAccessorType(XmlAccessType.FIELD)
public final class UserPreferences implements Serializable {

	@XmlElement(name = "e_hi")
	@XmlJavaTypeAdapter(BooleanDefaultTrueAdapter.class)
	private boolean showCqlCompletionHint = true;

	@XmlElement(name = "e_he")
	@XmlJavaTypeAdapter(BooleanDefaultTrueAdapter.class)
	private boolean showCqlHelp = true;

	@XmlElement(name = "i_hi")
	@XmlJavaTypeAdapter(BooleanDefaultTrueAdapter.class)
	private boolean importIncludeInHistory = true;

	@XmlElement(name = "i_ce")
	@XmlJavaTypeAdapter(BooleanDefaultTrueAdapter.class)
	private boolean importContinueWithErrors = false;

	public boolean isShowCqlCompletionHint() {
		return showCqlCompletionHint;
	}

	public void setShowCqlCompletionHint(boolean showCqlCompletionHint) {
		this.showCqlCompletionHint = showCqlCompletionHint;
	}

	public boolean isShowCqlHelp() {
		return showCqlHelp;
	}

	public void setShowCqlHelp(boolean showCqlHelp) {
		this.showCqlHelp = showCqlHelp;
	}

	@Override
	public String toString() {
		return Objects.toStringHelper(this).add("showCqlCompletionHint", showCqlCompletionHint)
				.add("showCqlHelp", showCqlHelp).add("importIncludeInHistory", importIncludeInHistory)
				.add("importContinueWithErrors", importContinueWithErrors).toString();
	}

	public boolean isImportIncludeInHistory() {
		return importIncludeInHistory;
	}

	public void setImportIncludeInHistory(boolean importIncludeInHistory) {
		this.importIncludeInHistory = importIncludeInHistory;
	}

	public boolean isImportContinueWithErrors() {
		return importContinueWithErrors;
	}

	public void setImportContinueWithErrors(boolean importContinueWithErrors) {
		this.importContinueWithErrors = importContinueWithErrors;
	}

	@Override
	public int hashCode() {
		return java.util.Objects
				.hash(showCqlCompletionHint, showCqlHelp, importIncludeInHistory, importContinueWithErrors);
	}

	@Override
	public boolean equals(Object obj) {
		if (obj == null || getClass() != obj.getClass()) {
			return false;
		}
		final UserPreferences other = (UserPreferences) obj;
		return java.util.Objects.equals(showCqlCompletionHint, other.showCqlCompletionHint) &&
				java.util.Objects.equals(showCqlHelp, other.showCqlHelp) &&
				java.util.Objects.equals(importIncludeInHistory, other.importIncludeInHistory) &&
				java.util.Objects.equals(importContinueWithErrors, other.importContinueWithErrors);
	}
}

/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.cyclop.model;

import java.io.Serializable;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

import org.cyclop.model.adapter.BooleanDefaultTrueAdapter;

import com.google.common.base.Objects;

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

	@XmlElement(name = "e_ro")
	private int resultOrientation = 0;

	@XmlElement(name = "i_hi")
	@XmlJavaTypeAdapter(BooleanDefaultTrueAdapter.class)
	private boolean importIncludeInHistory = true;

	@XmlElement(name = "i_ce")
	@XmlJavaTypeAdapter(BooleanDefaultTrueAdapter.class)
	private boolean importContinueWithErrors = false;

	@XmlElement(name = "i_pa")
	@XmlJavaTypeAdapter(BooleanDefaultTrueAdapter.class)
	private boolean importParallel = false;

	@XmlElement(name = "p_ei")
	private long pagerEditorItems = 5;

	@XmlElement(name = "p_hi")
	private long pagerHistoryItems = 50;

	@XmlElement(name = "p_ii")
	private long pagerImportItems = 100;

	public long getPagerImportItems() {
		return pagerImportItems;
	}

	public UserPreferences setPagerImportItems(long pagerImportItems) {
		this.pagerImportItems = pagerImportItems;
		return this;
	}

	public boolean isShowCqlCompletionHint() {
		return showCqlCompletionHint;
	}

	public UserPreferences setShowCqlCompletionHint(boolean showCqlCompletionHint) {
		this.showCqlCompletionHint = showCqlCompletionHint;
		return this;
	}

	public boolean isShowCqlHelp() {
		return showCqlHelp;
	}

	public UserPreferences setShowCqlHelp(boolean showCqlHelp) {
		this.showCqlHelp = showCqlHelp;
		return this;
	}

	@Override
	public String toString() {
		return Objects.toStringHelper(this).add("showCqlCompletionHint", showCqlCompletionHint)
				.add("showCqlHelp", showCqlHelp).add("importIncludeInHistory", importIncludeInHistory)
				.add("importContinueWithErrors", importContinueWithErrors).add("pagerEditorItems", pagerEditorItems)
				.add("pagerHistoryItems", pagerHistoryItems).add("pagerImportItems", pagerImportItems)
				.add("resultOrientation", resultOrientation).toString();
	}

	public boolean isImportIncludeInHistory() {
		return importIncludeInHistory;
	}

	public UserPreferences setImportIncludeInHistory(boolean importIncludeInHistory) {
		this.importIncludeInHistory = importIncludeInHistory;
		return this;
	}

	public boolean isImportContinueWithErrors() {
		return importContinueWithErrors;
	}

	public UserPreferences setImportContinueWithErrors(boolean importContinueWithErrors) {
		this.importContinueWithErrors = importContinueWithErrors;
		return this;
	}

	public long getPagerEditorItems() {
		return pagerEditorItems;
	}

	public UserPreferences setPagerEditorItems(long pagerEditorItems) {
		this.pagerEditorItems = pagerEditorItems;
		return this;
	}

	public boolean isImportParallel() {
		return importParallel;
	}

	public UserPreferences setImportParallel(boolean importParallel) {
		this.importParallel = importParallel;
		return this;
	}

	public long getPagerHistoryItems() {
		return pagerHistoryItems;
	}

	public UserPreferences setPagerHistoryItems(long pagerHistoryItems) {
		this.pagerHistoryItems = pagerHistoryItems;
		return this;
	}

	@Override
	public int hashCode() {
		return java.util.Objects.hash(showCqlCompletionHint, showCqlHelp, importIncludeInHistory,
				importContinueWithErrors, pagerEditorItems, pagerHistoryItems, pagerImportItems, importParallel,
				resultOrientation);
	}

	@Override
	public boolean equals(Object obj) {
		if (obj == null || getClass() != obj.getClass()) {
			return false;
		}
		final UserPreferences other = (UserPreferences) obj;
		return java.util.Objects.equals(showCqlCompletionHint, other.showCqlCompletionHint)
				&& java.util.Objects.equals(showCqlHelp, other.showCqlHelp)
				&& java.util.Objects.equals(importIncludeInHistory, other.importIncludeInHistory)
				&& java.util.Objects.equals(importContinueWithErrors, other.importContinueWithErrors)
				&& java.util.Objects.equals(pagerEditorItems, other.pagerEditorItems)
				&& java.util.Objects.equals(pagerHistoryItems, other.pagerHistoryItems)
				&& java.util.Objects.equals(pagerImportItems, other.pagerImportItems)
				&& java.util.Objects.equals(importParallel, other.importParallel)
				&& java.util.Objects.equals(resultOrientation, other.resultOrientation);
	}
}

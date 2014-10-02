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
package org.cyclop.web.panels.queryimport;

import java.io.Serializable;

import com.google.common.base.MoreObjects;

/** @author Maciej Miklas */
public final class ImportOptions implements Serializable {
	private boolean includeInHistory = true;

	private boolean continueWithErrors = false;

	private boolean parallel = false;

	private String scriptFile;

	@Override
	public String toString() {
		return MoreObjects.toStringHelper(this).add("includeInHistory", includeInHistory)
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

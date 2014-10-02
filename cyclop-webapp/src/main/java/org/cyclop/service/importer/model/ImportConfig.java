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
package org.cyclop.service.importer.model;

import com.google.common.base.MoreObjects;

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
		return MoreObjects.toStringHelper(this).add("updateHistory", updateHistory)
				.add("continueWithErrors", continueWithErrors).toString();
	}
}

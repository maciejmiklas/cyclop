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
package org.cyclop.service.completion.intern.parser;

import org.cyclop.model.CqlPart;

/** @author Maciej Miklas */

public abstract class MarkerBasedCompletion implements CqlPartCompletion {

	private CqlPart startMarker = null;

	protected MarkerBasedCompletion(CqlPart startMarker) {
		if (startMarker == null) {
			throw new IllegalArgumentException("NUll start marker");
		}
		this.startMarker = startMarker;

	}

	public final CqlPart startMarker() {
		return startMarker;
	}
}

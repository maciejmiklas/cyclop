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

import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableSet;
import com.sun.istack.NotNull;

import java.io.Serializable;

import javax.validation.Valid;

/** @author Maciej Miklas */
public class FilterResult<T> implements Serializable {

	@NotNull
	@Valid
	public final ImmutableList<T> result;

	/** trimmed, lower case */
	@NotNull
	public final ImmutableSet<String> normalizedKeywords;

	public FilterResult(ImmutableList<T> result, ImmutableSet<String> normalizedKeywords) {
		this.result = result;
		this.normalizedKeywords = normalizedKeywords;
	}

	@Override
	public String toString() {
		return "FilterResult [result=" + result + ", normalizedKeywords=" + normalizedKeywords + "]";
	}

}

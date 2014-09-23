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
package org.cyclop.service.cassandra.impl;

import java.util.Optional;

import javax.inject.Named;

import net.jcip.annotations.NotThreadSafe;

import org.apache.commons.lang.Validate;
import org.cyclop.model.CqlKeySpace;
import org.cyclop.service.cassandra.QueryScope;
import org.springframework.context.annotation.Scope;
import org.springframework.context.annotation.ScopedProxyMode;

/** @author Maciej Miklas */
@NotThreadSafe
@Named
@Scope(value = "session", proxyMode = ScopedProxyMode.TARGET_CLASS)
public class QueryScopeImpl implements QueryScope {

	private Optional<CqlKeySpace> activeKeySpace = Optional.empty();

	public Optional<CqlKeySpace> getActiveKeySpace() {
		return activeKeySpace;
	}

	protected void setActiveKeySpace(Optional<CqlKeySpace> activeSpace) {
		Validate.notNull(activeSpace, "null activeSpace");
		this.activeKeySpace = activeSpace;
	}
}

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
package org.cyclop.service.queryprotocoling.impl;

import javax.inject.Named;
import javax.validation.constraints.NotNull;

import net.jcip.annotations.NotThreadSafe;

import org.cyclop.model.QueryEntry;
import org.cyclop.model.QueryHistory;
import org.cyclop.service.queryprotocoling.HistoryService;
import org.cyclop.validation.EnableValidation;
import org.springframework.context.annotation.Scope;
import org.springframework.context.annotation.ScopedProxyMode;

/** @author Maciej Miklas */
@NotThreadSafe
@Named
@Scope(value = "session", proxyMode = ScopedProxyMode.TARGET_CLASS)
@EnableValidation
public class HistoryServiceImpl extends AbstractQueryProtocolingService<QueryHistory> implements HistoryService {

	protected HistoryServiceImpl() {
	}

	@Override
	protected Class<QueryHistory> getClazz() {
		return QueryHistory.class;
	}

	@Override
	protected QueryHistory createEmpty() {
		return new QueryHistory();
	}

	@Override
	public void addAndStore(@NotNull QueryEntry entry) {
		QueryHistory hist = read();
		hist.add(entry);
		store(hist);
	}
}

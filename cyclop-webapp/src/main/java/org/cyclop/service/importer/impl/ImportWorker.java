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
package org.cyclop.service.importer.impl;

import com.datastax.driver.core.Session;
import com.datastax.driver.core.exceptions.DriverException;
import com.google.common.collect.ImmutableList;
import org.cyclop.model.CqlQuery;
import org.cyclop.model.QueryEntry;
import org.cyclop.model.QueryHistory;
import org.cyclop.model.exception.QueryException;
import org.cyclop.service.importer.ResultWriter;
import org.cyclop.service.importer.model.ImportConfig;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.concurrent.Callable;

/** @author Maciej Miklas */
class ImportWorker implements Callable<Void> {

	private final static Logger LOG = LoggerFactory.getLogger(ParallelQueryImporter.class);

	private final int offset;

	private final int amount;

	private final ImmutableList<CqlQuery> queries;

	private final ResultWriter resultWriter;

	private final StatsCollector status;

	private final ImportConfig iconfig;

	private final Session session;

	private final QueryHistory history;

	// TODO to many paraeters - use builder pattern
	ImportWorker(int offset, int amount, ImmutableList<CqlQuery> queries, StatsCollector status, ImportConfig iconfig,
				 ResultWriter resultWriter, Session session, QueryHistory history) {
		this.offset = offset;
		this.amount = amount;
		this.queries = queries;
		this.status = status;
		this.iconfig = iconfig;
		this.resultWriter = resultWriter;
		this.session = session;
		this.history = history;
	}

	@Override
	public Void call() throws Exception {
		LOG.debug("Starting import thread with offset: {} for amount {}", offset, amount);

		int lastQueryIndex = offset + amount;
		for (int queryIdx = offset; queryIdx < lastQueryIndex; queryIdx++) {
			if (!canContinue()) {
				LOG.debug("Breaking import due to query execution error");
				return null;
			}
			process(queryIdx);
		}
		return null;
	}

	private void process(int queryIdx) {
		CqlQuery query = queries.get(queryIdx);

		long startTime = System.currentTimeMillis();
		try {
			LOG.debug("Executing {} with offset {}", query, queryIdx);
			session.execute(query.part);

			long runTime = System.currentTimeMillis() - startTime;
			if (iconfig.isUpdateHistory()) {
				QueryEntry entry = new QueryEntry(query, runTime);
				history.add(entry);
			}
			resultWriter.success(query, runTime);
			status.success.getAndIncrement();
		} catch (DriverException e) {
			LOG.debug(e.getMessage());
			LOG.trace(e.getMessage(), e);

			status.error.getAndIncrement();
			resultWriter.error(query, new QueryException(e.getMessage(), e), System.currentTimeMillis() - startTime);
		} catch (Exception e) {
			LOG.info("Unknown error while executing parallel import for: " + query + ", Msg:" + e.getMessage());
			LOG.trace(e.getMessage(), e);

			resultWriter.unknownError(query, e, System.currentTimeMillis() - startTime);
			status.error.getAndIncrement();
		}
	}

	private boolean canContinue() {
		int errors = status.error.get();
		boolean can = errors == 0 || (errors > 0 && iconfig.isContinueWithErrors());
		return can;
	}
}

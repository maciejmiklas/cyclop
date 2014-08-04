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

import java.util.ArrayList;
import java.util.List;
import java.util.Scanner;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Future;

import javax.inject.Inject;
import javax.inject.Named;

import org.apache.commons.lang.StringUtils;
import org.apache.commons.lang3.time.StopWatch;
import org.cyclop.model.CqlQuery;
import org.cyclop.model.CqlQueryType;
import org.cyclop.model.QueryHistory;
import org.cyclop.service.cassandra.CassandraSession;
import org.cyclop.service.importer.QueryImporter;
import org.cyclop.service.importer.ResultWriter;
import org.cyclop.service.importer.model.ImportConfig;
import org.cyclop.service.queryprotocoling.HistoryService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.datastax.driver.core.Session;
import com.google.common.collect.ImmutableList;

/** @author Maciej Miklas */
@Named(QueryImporter.IMPL_PARALLEL)
public class ParallelQueryImporter extends AbstractImporter {

	private final static Logger LOG = LoggerFactory.getLogger(ParallelQueryImporter.class);

	@Inject
	protected HistoryService historyService;

	@Inject
	private CassandraSession session;

	@Inject
	@Named("importExecutor")
	private ExecutorService executor;

	@Override
	void execImport(Scanner scanner, ResultWriter resultWriter, StatsCollector status, ImportConfig iconfig) {
		ImmutableList<CqlQuery> queries = parse(scanner);
		if (queries.isEmpty()) {
			LOG.debug("No data to import");
			return;
		}

		QueryHistory history = historyService.read();

		List<Future<Void>> futures = startWorkers(queries, resultWriter, status, iconfig, history);
		waitForImport(futures);

		if (iconfig.isUpdateHistory()) {
			historyService.store(history);
		}
	}

	private List<Future<Void>> startWorkers(ImmutableList<CqlQuery> queries, ResultWriter resultWriter,
											StatsCollector status, ImportConfig iconfig, QueryHistory history) {

		final int queriesSize = queries.size();
		final int proThread = Math.max(1, queriesSize / conf.queryImport.maxThreadsProImport);

		LOG.debug("Starting parallel import for {} queries, {} pro thread", queriesSize, proThread);
		Session cassSession = session.getSession();

		List<Future<Void>> futures = new ArrayList<>();
		int startIndex = 0;
		int remaining = queriesSize;
		while (remaining > 0) {
			remaining -= proThread;
			int amount = remaining > 0 ? proThread : remaining + proThread;

			LOG.debug("Starting thread with start index: {} and amount: {}, remaining: {}", startIndex, amount,
					remaining);

			ImportWorker task = new ImportWorker(startIndex, amount, queries, status, iconfig, resultWriter,
					cassSession, history);

			Future<Void> future = executor.submit(task);
			futures.add(future);
			startIndex += amount;
		}
		return futures;
	}

	private void waitForImport(List<Future<Void>> futures) {
		LOG.debug("Tasks submitted - waiting for results");
		for (Future<Void> future : futures) {
			try {
				future.get();
			} catch (InterruptedException e) {
				Thread.interrupted();
				LOG.warn("Import executor interrupted", e);
			} catch (Exception e) {
				LOG.error("Import executor error", e);
			}
		}
	}

	private ImmutableList<CqlQuery> parse(Scanner scanner) {
		StopWatch timer = null;
		if (LOG.isDebugEnabled()) {
			timer = new StopWatch();
			timer.start();
		}

		ImmutableList.Builder<CqlQuery> build = ImmutableList.builder();
		while (scanner.hasNext()) {
			String nextStr = StringUtils.trimToNull(scanner.next());
			if (nextStr == null) {
				continue;
			}
			CqlQuery query = new CqlQuery(CqlQueryType.UNKNOWN, nextStr);
			build.add(query);
		}
		ImmutableList<CqlQuery> res = build.build();

		if (LOG.isDebugEnabled()) {
			timer.stop();
			LOG.debug("Parsed {} queries in {}", res.size(), timer.toString());
		}
		return res;
	}

}

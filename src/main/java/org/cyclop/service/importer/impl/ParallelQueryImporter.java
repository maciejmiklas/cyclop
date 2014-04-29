package org.cyclop.service.importer.impl;

import com.datastax.driver.core.Session;
import com.google.common.collect.ImmutableList;
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

import javax.inject.Inject;
import javax.inject.Named;
import java.util.ArrayList;
import java.util.List;
import java.util.Scanner;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Future;

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
	void execImport(Scanner scanner, ResultWriter resultWriter, StatusCollector status, ImportConfig iconfig) {
		ImmutableList<CqlQuery> queries = parse(scanner);
		if (queries.isEmpty()) {
			LOG.debug("No data to import");
			return;
		}

		final int queriesSize = queries.size();
		final int proThread = Math.max(1, queriesSize / conf.queryImport.maxThreadsProImport);
		final int mod = queriesSize % conf.queryImport.maxThreadsProImport;
		int startIndex = 0;

		LOG.debug("Starting parallel import for {} queries, {} pro thread", queriesSize, proThread);
		Session cassSession = session.getSession();
		List<Future<Void>> futures = new ArrayList<>();
		QueryHistory history = historyService.read();

		for (int thrNr = 1; thrNr <= conf.queryImport.maxThreadsProImport; thrNr++) {
			int amount;
			if (thrNr == 1) {
				amount = proThread + mod;
			} else {
				amount = proThread;
			}

			if (amount + startIndex > queriesSize) {
				thrNr = Integer.MAX_VALUE;
				amount = queriesSize - startIndex;
			}

			LOG.debug("Starting thread nr: {} with start index: {} and amount: {}", thrNr, startIndex, amount);

			ImportWorker task = new ImportWorker(startIndex, amount, queries, status, iconfig, resultWriter,
					cassSession, history);

			Future<Void> future = executor.submit(task);
			futures.add(future);

			startIndex += amount;
		}

		waitForImport(futures);

		if (iconfig.isUpdateHistory()) {
			historyService.store(history);
		}
		LOG.debug("Tasks submitted - waiting for results");
	}

	private void waitForImport(List<Future<Void>> futures) {
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

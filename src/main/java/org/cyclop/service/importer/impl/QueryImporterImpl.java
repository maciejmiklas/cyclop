package org.cyclop.service.importer.impl;

import org.apache.commons.lang.StringUtils;
import org.apache.commons.lang3.time.StopWatch;
import org.cyclop.common.AppConfig;
import org.cyclop.model.CqlQuery;
import org.cyclop.model.CqlQueryType;
import org.cyclop.model.exception.QueryException;
import org.cyclop.service.cassandra.QueryService;
import org.cyclop.service.importer.QueryImporter;
import org.cyclop.service.importer.ResultWritter;
import org.cyclop.service.importer.model.ImportStats;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.inject.Inject;
import javax.inject.Named;
import java.io.InputStream;
import java.util.Scanner;

/** @author Maciej Miklas */
@Named
public class QueryImporterImpl implements QueryImporter {

	private final static Logger LOG = LoggerFactory.getLogger(QueryImporterImpl.class);

	@Inject
	private QueryService queryService;

	@Inject
	private AppConfig conf;

	@Override
	public ImportStats importScript(InputStream input, ResultWritter resultWritter, boolean updateHistory,
									boolean continueWithErrors) {

		LOG.debug("Starting query import");
		StopWatch timeScript = new StopWatch();
		timeScript.start();
		int successCount = 0;
		int errorCount = 0;
		Scanner scanner = new Scanner(input, conf.cqlImport.encoding);
		scanner.useDelimiter(conf.cqlImport.listSeparatorRegEx);

		while (scanner.hasNext()) {
			String nextStr = StringUtils.trimToNull(scanner.next());
			if (nextStr == null) {
				continue;
			}
			CqlQuery query = new CqlQuery(CqlQueryType.UNKNOWN, nextStr);
			long startTime = System.currentTimeMillis();
			try {
				LOG.debug("Executing: {}", query);
				queryService.executeSimple(query, updateHistory);
				resultWritter.success(query, System.currentTimeMillis() - startTime);
				successCount++;
			} catch (QueryException e) {
				errorCount++;
				LOG.debug(e.getMessage());
				LOG.trace(e.getMessage(), e);
				resultWritter.error(query, e, System.currentTimeMillis() - startTime);

				if (!continueWithErrors) {
					LOG.debug("Breaking import due to an error");
					break;
				}
			}
		}
		timeScript.stop();
		ImportStats stats = new ImportStats(timeScript, successCount, errorCount);

		LOG.debug("Import done: {}", stats);
		return stats;
	}

}

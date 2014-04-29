package org.cyclop.service.importer.impl;

import org.apache.commons.lang3.time.StopWatch;
import org.cyclop.common.AppConfig;
import org.cyclop.service.importer.QueryImporter;
import org.cyclop.service.importer.ResultWriter;
import org.cyclop.service.importer.model.ImportConfig;
import org.cyclop.service.importer.model.ImportStats;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.inject.Inject;
import java.io.InputStream;
import java.util.Scanner;

/** @author Maciej Miklas */
abstract class AbstractImporter implements QueryImporter {
	private final static Logger LOG = LoggerFactory.getLogger(AbstractImporter.class);

	@Inject
	protected AppConfig conf;

	@Override
	public final ImportStats importScript(InputStream input, ResultWriter resultWriter, ImportConfig config) {
		LOG.debug("Starting query import");
		StopWatch timer = new StopWatch();
		timer.start();
		StatusCollector status = new StatusCollector();
		Scanner scanner = new Scanner(input, conf.queryImport.encoding);
		scanner.useDelimiter(conf.queryImport.listSeparatorRegEx);

		LOG.debug("Executing import");
		execImport(scanner, resultWriter, status, config);

		timer.stop();
		ImportStats stats = new ImportStats(timer, status.success.get(), status.error.get());

		LOG.debug("Import done: {}", stats);
		return stats;
	}

	abstract void execImport(Scanner scanner, ResultWriter resultWriter, StatusCollector status, ImportConfig config);

}

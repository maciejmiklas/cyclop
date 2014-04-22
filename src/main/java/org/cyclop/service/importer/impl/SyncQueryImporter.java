package org.cyclop.service.importer.impl;

import org.apache.commons.lang.StringUtils;
import org.cyclop.model.CqlQuery;
import org.cyclop.model.CqlQueryType;
import org.cyclop.model.exception.QueryException;
import org.cyclop.service.importer.QueryImporter;
import org.cyclop.service.importer.ResultWriter;
import org.cyclop.service.importer.model.ImportConfig;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.inject.Named;
import java.util.Scanner;

/** @author Maciej Miklas */
@Named(QueryImporter.IMPL_SYNC)
public class SyncQueryImporter extends AbstractImporter {

	private final static Logger LOG = LoggerFactory.getLogger(SyncQueryImporter.class);

	@Override
	void execImport(Scanner scanner, ResultWriter resultWriter, StatusCollector status, ImportConfig config) {
		while (scanner.hasNext()) {
			String nextStr = StringUtils.trimToNull(scanner.next());
			if (nextStr == null) {
				continue;
			}
			CqlQuery query = new CqlQuery(CqlQueryType.UNKNOWN, nextStr);
			long startTime = System.currentTimeMillis();
			try {
				LOG.debug("Executing: {}", query);
				queryService.executeSimple(query, config.isUpdateHistory());
				resultWriter.success(query, System.currentTimeMillis() - startTime);
				status.success.getAndIncrement();
			} catch (QueryException e) {
				status.error.getAndIncrement();
				LOG.debug(e.getMessage());
				LOG.trace(e.getMessage(), e);
				resultWriter.error(query, e, System.currentTimeMillis() - startTime);

				if (!config.isContinueWithErrors()) {
					LOG.debug("Breaking import due to an error");
					break;
				}
			}
		}
	}
}

package org.cyclop.web.panels.queryimport;

import com.google.common.collect.ImmutableList;
import org.cyclop.model.CqlQuery;
import org.cyclop.model.exception.QueryException;
import org.cyclop.service.importer.ResultWriter;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.UUID;

/** @author Maciej Miklas */
final class ImportResultWriter implements ResultWriter {

	private final static Logger LOG = LoggerFactory.getLogger(QueryImportPanel.class);

	private final ImmutableList.Builder<ImportResult> resultBuild = ImmutableList.builder();

	public synchronized ImmutableList<ImportResult> getResult() {
		return resultBuild.build();
	}

	@Override
	public synchronized void success(CqlQuery query, long runtime) {
		resultBuild.add(new ImportResult(query, runtime));
	}

	@Override
	public synchronized void error(CqlQuery query, QueryException error, long runtime) {
		resultBuild.add(new ImportResult(query, error.getMessage(), runtime));
	}

	@Override
	public synchronized void unknownError(CqlQuery query, Exception ex, long runtime) {
		String errorRef = UUID.randomUUID().toString();
		String errorMsg = "Unhandled exception, Error ID: " + errorRef;
		LOG.error(errorMsg, ex);
		resultBuild.add(new ImportResult(query, errorMsg, runtime));
	}

}

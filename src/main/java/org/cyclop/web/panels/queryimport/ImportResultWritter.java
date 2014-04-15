package org.cyclop.web.panels.queryimport;

import com.google.common.collect.ImmutableList;
import org.cyclop.model.CqlQuery;
import org.cyclop.model.exception.QueryException;
import org.cyclop.service.importer.ResultWritter;

/** @author Maciej Miklas */
final class ImportResultWritter implements ResultWritter {

	private final ImmutableList.Builder<ImportResult> resultBuild = ImmutableList.builder();

	@Override
	public void error(CqlQuery query, QueryException error, long runtime) {
		resultBuild.add(new ImportResult(query, error, runtime));
	}

	public ImmutableList<ImportResult> getResult() {
		return resultBuild.build();
	}

	@Override
	public void success(CqlQuery query, long runtime) {
		resultBuild.add(new ImportResult(query, runtime));
	}

}

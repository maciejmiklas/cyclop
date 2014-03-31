package org.cyclop.service.importer.impl;

import java.io.InputStream;
import java.util.Scanner;

import javax.inject.Inject;
import javax.inject.Named;

import org.apache.commons.lang.StringUtils;
import org.cyclop.common.AppConfig;
import org.cyclop.model.CqlQuery;
import org.cyclop.model.CqlQueryType;
import org.cyclop.model.exception.QueryException;
import org.cyclop.service.cassandra.QueryService;
import org.cyclop.service.importer.QueryImporter;
import org.cyclop.service.importer.ResultWritter;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.common.collect.ImmutableList;

/** @author Maciej Miklas */
@Named
public class QueryImporterImpl implements QueryImporter {

    private final static Logger LOG = LoggerFactory.getLogger(QueryImporterImpl.class);

    @Inject
    private QueryService queryService;

    @Inject
    private AppConfig conf;

    @Override
    public void importScript(InputStream input, ResultWritter resultWritter) {
	LOG.debug("Starting query import");
	ImmutableList<CqlQuery> queries = parse(input);
	execute(queries, resultWritter);
    }

    private void execute(ImmutableList<CqlQuery> queries, ResultWritter resultWritter) {
	for (CqlQuery query : queries) {
	    long startTime = System.currentTimeMillis();
	    try {
		LOG.debug("Executing: {}", query);
		queryService.execute(query);
		resultWritter.success(query, System.currentTimeMillis() - startTime);
	    }
	    catch (QueryException e) {
		LOG.debug(e.getMessage(), e);
		resultWritter.error(query, e, System.currentTimeMillis() - startTime);
	    }
	}
    }

    private ImmutableList<CqlQuery> parse(InputStream input) {
	Scanner scanner = new Scanner(input, conf.cqlImport.encoding);
	scanner.useDelimiter(conf.cqlImport.listSeparatorRegEx);

	ImmutableList.Builder<CqlQuery> resBuild = ImmutableList.builder();
	while (scanner.hasNext()) {
	    String nextStr = StringUtils.trimToNull(scanner.next());
	    if (nextStr == null) {
		continue;
	    }
	    resBuild.add(new CqlQuery(CqlQueryType.UNKNOWN, nextStr));
	}

	ImmutableList<CqlQuery> res = resBuild.build();
	LOG.trace("Parsed import: {}", res);
	return res;
    }
}

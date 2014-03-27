package org.cyclop.service.exporter;

import org.cyclop.model.CqlQuery;
import org.cyclop.model.CqlQueryResult;

/** @author Maciej Miklas */
public interface CsvQueryResultExporter {

	String exportAsCsv(CqlQuery query, CqlQueryResult result);
}

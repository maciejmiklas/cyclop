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
package org.cyclop.service.exporter.impl;

import java.io.ByteArrayOutputStream;
import java.io.OutputStream;
import java.io.PrintWriter;
import java.io.UnsupportedEncodingException;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import javax.inject.Inject;
import javax.inject.Named;

import org.cyclop.common.AppConfig;
import org.cyclop.model.CqlColumnValue;
import org.cyclop.model.CqlDataType;
import org.cyclop.model.CqlExtendedColumnName;
import org.cyclop.model.CqlQuery;
import org.cyclop.model.CqlQueryResult;
import org.cyclop.model.exception.ServiceException;
import org.cyclop.service.cassandra.QueryService;
import org.cyclop.service.converter.DataConverter;
import org.cyclop.service.converter.DataExtractor;
import org.cyclop.service.exporter.CsvQueryResultExporter;
import org.cyclop.validation.EnableValidation;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.datastax.driver.core.DataType;
import com.datastax.driver.core.Row;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableSet;

/** @author Maciej Miklas */
@Named
@EnableValidation
public class CsvQueryResultExporterImpl implements CsvQueryResultExporter {

	private final static Logger LOG = LoggerFactory.getLogger(CsvQueryResultExporterImpl.class);

	@Inject
	private DataExtractor extractor;

	@Inject
	private DataConverter converter;

	@Inject
	private AppConfig.QueryExport conf;

	@Inject
	private QueryService queryService;

	@Override
	public String exportAsCsv(CqlQuery query) {
		ByteArrayOutputStream bos = new ByteArrayOutputStream();
		exportAsCsv(query, bos);
		try {
			String resStr = new String(bos.toByteArray(), conf.encoding);
			return resStr;
		} catch (UnsupportedEncodingException e) {
			throw new ServiceException("Encoding: " + conf.encoding + " caused error during export: " + e.getMessage(),
					e);
		}
	}

	@Override
	public void exportAsCsv(CqlQuery query, OutputStream output) {
		LOG.debug("Starting CSV export for {}", query);

		PrintWriter out = new PrintWriter(output);
		CqlQueryResult result = queryService.execute(query, false);

		// header
		appendHeader(query, out);

		// column names
		ImmutableList<CqlExtendedColumnName> columns = result.rowMetadata.columns;
		appendColumns(out, columns);
		out.append(conf.separatorRow);

		// content
		for (Row row : result) {
			appendRow(out, row, columns);
			out.append(conf.separatorRow);
		}

		out.flush();
		out.close();
	}

	private void appendRow(PrintWriter out, Row row, List<CqlExtendedColumnName> cols) {
		LOG.debug("Appending next row");
		Iterator<CqlExtendedColumnName> it = cols.iterator();
		while (it.hasNext()) {
			CqlExtendedColumnName column = it.next();
			CqlDataType dataType = column.dataType;

			if (dataType.name == DataType.Name.SET || dataType.name == DataType.Name.LIST) {
				appendCollection(out, row, column);

			} else if (dataType.name == DataType.Name.MAP) {
				appendMap(out, row, column);
			} else {
				appendSingleValue(out, row, column);
			}

			if (it.hasNext()) {
				out.append(conf.separatorColumn);
			}
		}
	}

	private void appendMap(PrintWriter out, Row row, CqlExtendedColumnName column) {
		ImmutableSet<Map.Entry<CqlColumnValue, CqlColumnValue>> displayMap = extractor.extractMap(row, column)
				.entrySet();
		Iterator<Map.Entry<CqlColumnValue, CqlColumnValue>> it = displayMap.iterator();

		LOG.trace("Appending: {}", displayMap);

		StringBuilder mapBuf = new StringBuilder();
		while (it.hasNext()) {
			Map.Entry<CqlColumnValue, CqlColumnValue> entry = it.next();

			CqlColumnValue key = entry.getKey();
			String keyText = esc(converter.convert(key.value));
			mapBuf.append(keyText);

			mapBuf.append(conf.separatorMap);

			CqlColumnValue val = entry.getValue();
			String valText = esc(converter.convert(val.value));
			mapBuf.append(valText);

			if (it.hasNext()) {
				mapBuf.append(conf.separatorList);
			}
		}

		String mapVal = esc(mapBuf.toString());
		LOG.trace("Appended map: {}", mapVal);
		out.append(mapVal);
	}

	private void appendCollection(PrintWriter out, Row row, CqlExtendedColumnName column) {
		ImmutableList<CqlColumnValue> content = extractor.extractCollection(row, column);
		LOG.trace("Appending {}", content);
		Iterator<CqlColumnValue> contentIt = content.iterator();
		StringBuilder listBuild = new StringBuilder();
		while (contentIt.hasNext()) {
			CqlColumnValue cqlColumnValue = contentIt.next();
			String valText = esc(converter.convert(cqlColumnValue.value));
			listBuild.append(valText);
			if (contentIt.hasNext()) {
				listBuild.append(conf.separatorList);
			}
		}
		String colVal = esc(listBuild.toString());
		LOG.trace("Append collection: {}", colVal);
		out.append(colVal);
	}

	private void appendSingleValue(PrintWriter out, Row row, CqlExtendedColumnName column) {
		CqlColumnValue cqlColumnValue = extractor.extractSingleValue(row, column);
		String valText = esc(converter.convert(cqlColumnValue.value));
		LOG.trace("Append single value: {}", valText);
		out.append(valText);
	}

	private void appendHeader(CqlQuery query, PrintWriter out) {
		String headerVal = prep(query.part);
		LOG.trace("Append header: {}", headerVal);
		out.append(headerVal);
		out.append(conf.separatorQuery);
	}

	private void appendColumns(PrintWriter out, List<CqlExtendedColumnName> columns) {
		if (columns.isEmpty()) {
			return;
		}

		LOG.trace("Appending {}", columns);

		Iterator<CqlExtendedColumnName> commonColsIt = columns.iterator();
		while (commonColsIt.hasNext()) {
			CqlExtendedColumnName next = commonColsIt.next();
			out.append(prep(esc(next.toDisplayString())));

			if (commonColsIt.hasNext()) {
				out.append(conf.separatorColumn);
			}
		}
	}

	private String prep(String val) {
		if (val == null) {
			val = "";
		}
		if (conf.trim) {
			val = val.trim();
		}

		if (conf.removeCrChars) {
			val = val.replaceAll("[\n\r]", "");
		}

		return val;
	}

	private String esc(String val) {
		return conf.valueBracketStart + prep(val) + conf.valueBracketEnd;
	}
}

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

import com.datastax.driver.core.DataType;
import com.datastax.driver.core.Row;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableSet;
import org.cyclop.common.AppConfig;
import org.cyclop.model.CqlColumnValue;
import org.cyclop.model.CqlDataType;
import org.cyclop.model.CqlExtendedColumnName;
import org.cyclop.model.CqlQuery;
import org.cyclop.model.CqlQueryResult;
import org.cyclop.service.converter.DataConverter;
import org.cyclop.service.converter.DataExtractor;
import org.cyclop.service.exporter.CsvQueryResultExporter;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.inject.Inject;
import javax.inject.Named;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

/** @author Maciej Miklas */
@Named
public class CsvQueryResultExporterImpl implements CsvQueryResultExporter {

	private final static Logger LOG = LoggerFactory.getLogger(CsvQueryResultExporterImpl.class);

	@Inject
	private DataExtractor extractor;

	@Inject
	private DataConverter converter;

	@Inject
	private AppConfig.QueryExport conf;

	public String exportAsCsv(CqlQuery query, CqlQueryResult result) {
		LOG.debug("Starting CSV export for {}", query);

		StringBuilder buf = new StringBuilder();

		// header
		appendHeader(query, buf);

		// column names
		appendColumns(buf, result.columns);
		buf.append(conf.rowSeparator);

		// content
		for (Row row : result) {
			appendRow(buf, row, result.columns);
			buf.append(conf.rowSeparator);
		}

		LOG.trace("Created CSV: {}", buf);
		return buf.toString();
	}

	private void appendRow(StringBuilder buf, Row row, List<CqlExtendedColumnName> cols) {
		LOG.debug("Appending next row");
		Iterator<CqlExtendedColumnName> it = cols.iterator();
		while (it.hasNext()) {
			CqlExtendedColumnName column = it.next();
			CqlDataType dataType = column.dataType;

			if (dataType.name == DataType.Name.SET || dataType.name == DataType.Name.LIST) {
				appendCollection(buf, row, column);

			} else if (dataType.name == DataType.Name.MAP) {
				appendMap(buf, row, column);
			} else {
				appendSingleValue(buf, row, column);
			}

			if (it.hasNext()) {
				buf.append(conf.columnSeparator);
			}
		}
	}

	private void appendMap(StringBuilder buf, Row row, CqlExtendedColumnName column) {
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

			mapBuf.append(conf.mapSeparator);

			CqlColumnValue val = entry.getValue();
			String valText = esc(converter.convert(val.value));
			mapBuf.append(valText);

			if (it.hasNext()) {
				mapBuf.append(conf.listSeparator);
			}
		}

		String mapVal = esc(mapBuf.toString());
		LOG.trace("Appended map: {}", mapVal);
		buf.append(mapVal);
	}

	private void appendCollection(StringBuilder buf, Row row, CqlExtendedColumnName column) {
		ImmutableList<CqlColumnValue> content = extractor.extractCollection(row, column);
		LOG.trace("Appending {}", content);
		Iterator<CqlColumnValue> contentIt = content.iterator();
		StringBuilder listBuild = new StringBuilder();
		while (contentIt.hasNext()) {
			CqlColumnValue cqlColumnValue = contentIt.next();
			String valText = esc(converter.convert(cqlColumnValue.value));
			listBuild.append(valText);
			if (contentIt.hasNext()) {
				listBuild.append(conf.listSeparator);
			}
		}
		String colVal = esc(listBuild.toString());
		LOG.trace("Append collection: {}", colVal);
		buf.append(colVal);
	}

	private void appendSingleValue(StringBuilder buf, Row row, CqlExtendedColumnName column) {
		CqlColumnValue cqlColumnValue = extractor.extractSingleValue(row, column);
		String valText = esc(converter.convert(cqlColumnValue.value));
		LOG.trace("Append single value: {}", valText);
		buf.append(valText);
	}

	private void appendHeader(CqlQuery query, StringBuilder buf) {
		String headerVal = prep(query.part);
		LOG.trace("Append header: {}", headerVal);
		buf.append(headerVal);
		buf.append(conf.querySeparator);
	}

	private void appendColumns(StringBuilder buf, List<CqlExtendedColumnName> columns) {
		if (columns.isEmpty()) {
			return;
		}

		LOG.trace("Appending {}", columns);

		Iterator<CqlExtendedColumnName> commonColsIt = columns.iterator();
		while (commonColsIt.hasNext()) {
			CqlExtendedColumnName next = commonColsIt.next();
			buf.append(prep(esc(next.toDisplayString())));

			if (commonColsIt.hasNext()) {
				buf.append(conf.columnSeparator);
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

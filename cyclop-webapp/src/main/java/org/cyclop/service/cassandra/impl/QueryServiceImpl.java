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
package org.cyclop.service.cassandra.impl;

import static java.util.stream.Collectors.collectingAndThen;
import static java.util.stream.Collectors.toList;
import static org.cyclop.common.QueryHelper.extractSpace;
import static org.cyclop.common.QueryHelper.extractTableName;

import java.util.Iterator;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.stream.StreamSupport;

import javax.inject.Inject;
import javax.inject.Named;

import org.apache.commons.lang.StringUtils;
import org.apache.commons.lang.Validate;
import org.cyclop.common.AppConfig;
import org.cyclop.model.CqlColumnName;
import org.cyclop.model.CqlColumnType;
import org.cyclop.model.CqlDataType;
import org.cyclop.model.CqlExtendedColumnName;
import org.cyclop.model.CqlIndex;
import org.cyclop.model.CqlKeySpace;
import org.cyclop.model.CqlKeyword;
import org.cyclop.model.CqlPartitionKey;
import org.cyclop.model.CqlQuery;
import org.cyclop.model.CqlQueryResult;
import org.cyclop.model.CqlQueryType;
import org.cyclop.model.CqlRowMetadata;
import org.cyclop.model.CqlTable;
import org.cyclop.model.QueryEntry;
import org.cyclop.model.exception.QueryException;
import org.cyclop.service.cassandra.QueryService;
import org.cyclop.service.queryprotocoling.HistoryService;
import org.cyclop.validation.EnableValidation;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.datastax.driver.core.ColumnDefinitions;
import com.datastax.driver.core.DataType;
import com.datastax.driver.core.ResultSet;
import com.datastax.driver.core.Row;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableMap;
import com.google.common.collect.ImmutableSortedSet;

/** @author Maciej Miklas */
@EnableValidation
@Named
@CassandraVersionQualifier(CassandraVersion.VER_2_x)
class QueryServiceImpl implements QueryService {

	private final static Logger LOG = LoggerFactory.getLogger(QueryServiceImpl.class);

	@Inject
	protected AppConfig config;

	@Inject
	protected CassandraSessionImpl session;

	@Inject
	protected QueryScopeImpl queryScope;

	@Inject
	private HistoryService historyService;

	@Override
	public boolean checkTableExists(CqlTable table) {
		Validate.notNull(table, "null CqlTable");

		StringBuilder cql = new StringBuilder("select columnfamily_name from system.schema_columnfamilies");
		cql.append(" where columnfamily_name='").append(table.partLc).append("' allow filtering");

		Optional<ResultSet> result = executeSilent(cql.toString());
		boolean tableExists = result.filter(r -> !r.isExhausted()).isPresent();
		return tableExists;
	}

	@Override
	public ImmutableSortedSet<CqlIndex> findAllIndexes(Optional<CqlKeySpace> keySpace) {

		StringBuilder cql = new StringBuilder("SELECT index_name FROM system.schema_columns");
		if (keySpace.isPresent()) {
			cql.append(" where keyspace_name='").append(keySpace.get().partLc).append("'");
		}

		Optional<ResultSet> result = executeSilent(cql.toString());
		if (!result.isPresent()) {
			LOG.debug("No indexes found for keyspace: " + keySpace);
			return ImmutableSortedSet.of();
		}

		ImmutableSortedSet<CqlIndex> indexesResp = StreamSupport.stream(result.get().spliterator(), false)
				.map(r -> r.getString("index_name")).map(StringUtils::trimToNull).filter(Objects::nonNull)
				.map(CqlIndex::new).collect(collectingAndThen(toList(), ImmutableSortedSet::copyOf));
		return indexesResp;
	}

	@Override
	public ImmutableSortedSet<CqlKeySpace> findAllKeySpaces() {

		Optional<ResultSet> result = executeSilent("select keyspace_name from system.schema_keyspaces");
		if (!result.isPresent()) {
			LOG.debug("Cannot readIdentifier keyspace info");
			return ImmutableSortedSet.of();
		}

		ImmutableSortedSet.Builder<CqlKeySpace> keyspaces = ImmutableSortedSet.naturalOrder();
		for (Row row : result.get()) {
			keyspaces.add(new CqlKeySpace(row.getString("keyspace_name")));
		}
		return keyspaces.build();
	}

	@Override
	public ImmutableSortedSet<CqlTable> findTableNames(Optional<CqlKeySpace> keySpace) {

		StringBuilder cql = new StringBuilder("select columnfamily_name from system.schema_columnfamilies");
		if (keySpace.isPresent()) {
			cql.append(" where keyspace_name='").append(keySpace.get().partLc).append("'");
		}
		Optional<ResultSet> result = executeSilent(cql.toString());
		if (!result.isPresent()) {
			LOG.debug("No table names found for keyspace: " + keySpace);
			return ImmutableSortedSet.of();
		}

		ImmutableSortedSet.Builder<CqlTable> tables = ImmutableSortedSet.naturalOrder();
		for (Row row : result.get()) {
			String columnFamily = row.getString("columnfamily_name");
			columnFamily = StringUtils.trimToNull(columnFamily);
			if (columnFamily == null) {
				continue;
			}
			CqlTable table = new CqlTable(columnFamily);
			tables.add(table);
		}
		return tables.build();
	}

	private void setActiveKeySpace(CqlQuery query) {
		Optional<CqlKeySpace> space = extractSpace(query);
		queryScope.setActiveKeySpace(space);
	}

	@Override
	public CqlQueryResult execute(CqlQuery query) {
		return execute(query, true);
	}

	@Override
	public void executeSimple(CqlQuery query, boolean updateHistory) {
		long startTime = System.currentTimeMillis();
		execute(query.part);
		if (updateHistory) {
			updateHistory(query, startTime);
		}
	}

	@Override
	public CqlQueryResult execute(CqlQuery query, boolean updateHistory) {
		long startTime = System.currentTimeMillis();
		CqlQueryResult result = executeIntern(query);

		if (updateHistory) {
			updateHistory(query, startTime);
		}
		return result;
	}

	private CqlQueryResult executeIntern(CqlQuery query) {
		LOG.debug("Executing CQL: {}", query);
		if (query.type == CqlQueryType.USE) {
			setActiveKeySpace(query);
		}

		ResultSet cqlResult = execute(query.part);
		if (cqlResult == null || cqlResult.isExhausted()) {
			return CqlQueryResult.EMPTY;
		}

		Map<String, CqlColumnType> typeMap = createTypeMap(query);
		Row firstRow = cqlResult.one();
		CqlRowMetadata rowMetadata = extractRowMetadata(firstRow, typeMap);

		RowIterator rowIterator = new RowIterator(cqlResult.iterator(), firstRow);
		CqlQueryResult result = new CqlQueryResult(rowIterator, rowMetadata);
		return result;
	}

	private void updateHistory(CqlQuery query, long startTime) {
		long runTime = System.currentTimeMillis() - startTime;
		QueryEntry entry = new QueryEntry(query, runTime);
		historyService.addAndStore(entry);
	}

	private CqlRowMetadata extractRowMetadata(Row row, Map<String, CqlColumnType> typeMap) {

		// collect and count all columns
		ColumnDefinitions definitions = row.getColumnDefinitions();
		ImmutableList.Builder<CqlExtendedColumnName> columnsBuild = ImmutableList.builder();
		CqlPartitionKey partitionKey = null;
		for (int colIndex = 0; colIndex < definitions.size(); colIndex++) {
			if (colIndex > config.cassandra.columnsLimit) {
				LOG.debug("Reached columns limit: {}", config.cassandra.columnsLimit);
				break;
			}
			if (row.isNull(colIndex)) {
				continue;
			}
			DataType dataType = definitions.getType(colIndex);
			String columnNameText = definitions.getName(colIndex);
			CqlColumnType columnType = typeMap.get(columnNameText.toLowerCase());
			if (columnType == null) {
				columnType = CqlColumnType.REGULAR;
				LOG.debug("Column type not found for: {} - using regular", columnNameText);
			}
			CqlExtendedColumnName columnName = new CqlExtendedColumnName(columnType, CqlDataType.create(dataType),
					columnNameText);
			if (columnType == CqlColumnType.PARTITION_KEY) {
				partitionKey = CqlPartitionKey.fromColumn(columnName);
			}
			columnsBuild.add(columnName);
		}

		CqlRowMetadata metadata = new CqlRowMetadata(columnsBuild.build(), partitionKey);
		return metadata;
	}

	protected ImmutableMap<String, CqlColumnType> createTypeMap(CqlQuery query) {

		Optional<CqlTable> table = extractTableName(CqlKeyword.Def.FROM.value, query);
		if (!table.isPresent()) {
			LOG.warn("Could not extract table name from: {}. Column type information is not available.");
			return ImmutableMap.of();
		}

		Optional<ResultSet> result = executeSilent("select column_name, type from system.schema_columns where "
				+ "columnfamily_name='" + table.get().part + "' allow filtering");
		if (!result.isPresent()) {
			LOG.warn("Could not readIdentifier types for columns of table: " + table);
			return ImmutableMap.of();
		}

		ImmutableMap.Builder<String, CqlColumnType> typesBuild = ImmutableMap.builder();
		for (Row row : result.get()) {
			String typeText = StringUtils.trimToNull(row.getString("type"));
			String name = StringUtils.trimToNull(row.getString("column_name"));
			if (typeText == null || name == null) {
				continue;
			}
			CqlColumnType type = extractType(typeText);
			typesBuild.put(name.toLowerCase(), type);
		}
		ImmutableMap<String, CqlColumnType> typesMap = typesBuild.build();
		return typesMap;
	}

	@Override
	public ImmutableSortedSet<CqlColumnName> findColumnNames(Optional<CqlTable> table) {

		StringBuilder buf = new StringBuilder("select column_name from system.schema_columns");
		if (table.isPresent()) {
			buf.append(" where columnfamily_name='");
			buf.append(table.get().partLc);
			buf.append("'");
		}
		buf.append(" limit ");
		buf.append(config.cassandra.columnsLimit);
		buf.append(" allow filtering");

		Optional<ResultSet> result = executeSilent(buf.toString());
		if (!result.isPresent()) {
			LOG.warn("Cannot readIdentifier column names");
			return ImmutableSortedSet.of();
		}

		ImmutableSortedSet.Builder<CqlColumnName> cqlColumnNames = ImmutableSortedSet.naturalOrder();
		for (Row row : result.get()) {
			String name = StringUtils.trimToNull(row.getString("column_name"));
			if (name == null) {
				continue;
			}
			cqlColumnNames.add(new CqlColumnName(CqlDataType.create(DataType.text()), name));
		}

		loadPartitionKeyNames(table, cqlColumnNames);

		return cqlColumnNames.build();
	}

	// required only for cassandra 1.x
	protected void loadPartitionKeyNames(Optional<CqlTable> table,
			ImmutableSortedSet.Builder<CqlColumnName> cqlColumnNames) {

	}

	protected CqlColumnType extractType(String typeText) {

		CqlColumnType type;
		try {
			type = CqlColumnType.valueOf(typeText.toUpperCase());
		} catch (IllegalArgumentException ia) {
			LOG.warn("Read unsupported column type: {}", typeText, ia);
			type = CqlColumnType.REGULAR;
		}

		return type;
	}

	@Override
	public ImmutableSortedSet<CqlColumnName> findAllColumnNames() {
		return findColumnNames(Optional.empty());
	}

	protected Optional<ResultSet> executeSilent(String cql) {

		LOG.debug("Executing: {}", cql);
		ResultSet resultSet = null;
		try {
			resultSet = session.getSession().execute(cql);
		} catch (Exception e) {
			LOG.warn("Error executing CQL: '" + cql + "', reason: " + e.getMessage());
			LOG.debug(e.getMessage(), e);
		}
		return Optional.ofNullable(resultSet);
	}

	protected ResultSet execute(String cql) {

		LOG.debug("Executing: {} ", cql);
		ResultSet resultSet;
		try {
			resultSet = session.getSession().execute(cql);
		} catch (Exception e) {
			throw new QueryException("Error executing CQL: '" + cql + "', reason: " + e.getMessage(), e);
		}
		return resultSet;
	}

	private class RowIterator implements Iterator<Row> {

		private final Iterator<Row> wrapped;

		private final Row firstRow;

		private int read = 0;

		private RowIterator(Iterator<Row> wrapped, Row firstRow) {
			this.wrapped = wrapped;
			this.firstRow = firstRow;
		}

		@Override
		public boolean hasNext() {
			boolean has;
			if (read == 0 && firstRow != null) {
				has = true;

			} else {
				has = wrapped.hasNext();
			}
			return has;
		}

		@Override
		public Row next() {
			Row next = read == 0 ? firstRow : wrapped.next();

			if (next != null) {
				read++;
			}
			return next;
		}

		@Override
		public void remove() {
			throw new UnsupportedOperationException("Remove is not supported");
		}
	}

}

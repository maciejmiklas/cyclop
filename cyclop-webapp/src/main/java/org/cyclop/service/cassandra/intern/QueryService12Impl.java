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
package org.cyclop.service.cassandra.intern;

import static org.cyclop.common.QueryHelper.extractTableName;

import java.util.Arrays;
import java.util.Optional;

import javax.inject.Named;

import org.apache.commons.lang.StringUtils;
import org.cyclop.model.CassandraVersion;
import org.cyclop.model.CqlColumnName;
import org.cyclop.model.CqlColumnType;
import org.cyclop.model.CqlDataType;
import org.cyclop.model.CqlKeyword;
import org.cyclop.model.CqlQuery;
import org.cyclop.model.CqlTable;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.datastax.driver.core.DataType;
import com.datastax.driver.core.ResultSet;
import com.datastax.driver.core.Row;
import com.google.common.collect.ImmutableMap;
import com.google.common.collect.ImmutableSet;
import com.google.common.collect.ImmutableSortedSet;

/**
 * Fallback for Cassandra 1.2
 * 
 * @author Maciej Miklas
 */
@Named
@CassandraVersionQualifier(CassandraVersion.VER_1_2)
class QueryService12Impl extends QueryServiceImpl {

	private final static Logger LOG = LoggerFactory.getLogger(QueryService12Impl.class);

	private ImmutableSet<String> findPartitionKeyNamesLc(CqlTable table) {

		Optional<ResultSet> result = executeSilent("select key_aliases FROM system.schema_columnfamilies where "
				+ "columnfamily_name='" + table.part + "' allow filtering");
		if (!result.isPresent()) {
			LOG.warn("Cannot find partition key info");
			return ImmutableSet.of();
		}

		ImmutableSet.Builder<String> keys = ImmutableSet.builder();
		for (Row row : result.get()) {
			String aliases = StringUtils.trimToNull(row.getString("key_aliases"));
			if (aliases == null || aliases.length() < 5) {// ["id"]
				continue;
			}

			String aliasesPure = aliases.substring(2, aliases.length() - 2);
			Arrays.asList(aliasesPure.split(",")).forEach(alias -> keys.add(alias.trim().toLowerCase()));
		}

		ImmutableSet<String> res = keys.build();
		LOG.debug("Found key name(s):{} for table: {}", res, table);
		return res;
	}

	@Override
	protected void loadPartitionKeyNames(Optional<CqlTable> table,
			ImmutableSortedSet.Builder<CqlColumnName> cqlColumnNames) {
		if (!table.isPresent()) {
			return;
		}
		findPartitionKeyNamesLc(table.get())
				.forEach(
						partitionKey -> cqlColumnNames.add(new CqlColumnName(CqlDataType.create(DataType.text()),
								partitionKey)));
	}

	protected ImmutableMap<String, CqlColumnType> createTypeMap(CqlQuery query) {

		Optional<CqlTable> table = extractTableName(CqlKeyword.Def.FROM.value, query);
		if (!table.isPresent()) {
			LOG.warn("Could not extract table name from: {}. Column type information is not available.");
			return ImmutableMap.of();
		}

		ImmutableMap.Builder<String, CqlColumnType> types = ImmutableMap.builder();
		findPartitionKeyNamesLc(table.get()).forEach(pk -> types.put(pk, CqlColumnType.PARTITION_KEY));

		ResultSet result = execute("select column_name from system.schema_columns where columnfamily_name='"
				+ table.get().part + "' allow filtering");
		for (Row row : result) {
			String name = StringUtils.trimToNull(row.getString("column_name"));
			if (name == null) {
				continue;
			}
			String nameLc = name.toLowerCase();
			types.put(nameLc, CqlColumnType.REGULAR);
		}
		return types.build();
	}
}

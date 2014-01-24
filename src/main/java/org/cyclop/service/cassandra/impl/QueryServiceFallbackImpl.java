package org.cyclop.service.cassandra.impl;

import com.datastax.driver.core.DataType;
import com.datastax.driver.core.ResultSet;
import com.datastax.driver.core.Row;
import com.google.common.collect.ImmutableMap;
import com.google.common.collect.ImmutableSet;
import com.google.common.collect.ImmutableSortedSet;
import java.util.Set;
import javax.inject.Named;
import org.apache.commons.lang.StringUtils;
import org.cyclop.model.CqlColumnName;
import org.cyclop.model.CqlColumnType;
import org.cyclop.model.CqlKeyword;
import org.cyclop.model.CqlQuery;
import org.cyclop.model.CqlTable;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import static org.cyclop.common.QueryHelper.extractTableName;

/**
 * @author Maciej Miklas
 */
@Named
@CassandraVersionQualifier(CassandraVersion.VER_1_x)
class QueryServiceFallbackImpl extends QueryServiceImpl {

    private final static Logger LOG = LoggerFactory.getLogger(QueryServiceFallbackImpl.class);

    private ImmutableSet<String> findPartitionKeyNamesLc(CqlTable table) {

        ResultSet result = executeSilent("select key_aliases FROM system.schema_columnfamilies where " +
                "columnfamily_name='" + table.part + "' allow filtering");
        if (result == null) {
            LOG.warn("Cannot find partition key info");
            return ImmutableSet.of();
        }

        ImmutableSet.Builder<String> keys = ImmutableSet.builder();
        for (Row row : result) {
            String aliases = StringUtils.trimToNull(row.getString("key_aliases"));
            if (aliases == null || aliases.length() < 5) {// ["id"]
                continue;
            }

            String aliasesPure = aliases.substring(2, aliases.length() - 2);
            for (String alias : aliasesPure.split(",")) {
                keys.add(alias.trim().toLowerCase());
            }
        }

        ImmutableSet<String> res = keys.build();
        LOG.debug("Found key name(s):{} for table: {}", res, table);
        return res;
    }

    @Override
    protected void loadPartitionKeyNames(CqlTable table, ImmutableSortedSet.Builder<CqlColumnName> cqlColumnNames) {
        if (table == null) {
            return;
        }
        ImmutableSet<String> partitionKeys = findPartitionKeyNamesLc(table);
        for (String partitionKey : partitionKeys) {
            cqlColumnNames.add(new CqlColumnName(DataType.text(), partitionKey));
        }
    }

    protected ImmutableMap<String, CqlColumnType> createTypeMap(CqlQuery query) {

        CqlTable table = extractTableName(CqlKeyword.Def.FROM.value, query);
        if (table == null) {
            LOG.warn("Could not extract table name from: {}. Column type information is not available.");
            return ImmutableMap.of();
        }

        ImmutableMap.Builder<String, CqlColumnType> types = ImmutableMap.builder();
        Set<String> partitionKeys = findPartitionKeyNamesLc(table);
        for (String pk : partitionKeys) {
            types.put(pk, CqlColumnType.PARTITION_KEY);
        }

        ResultSet result = execute("select column_name from system.schema_columns where columnfamily_name='" + table
                .part + "' allow filtering");

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

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
package org.cyclop.model;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import java.time.LocalDateTime;
import java.util.List;
import java.util.UUID;

import javax.inject.Inject;

import org.cyclop.service.cassandra.QueryService;
import org.cyclop.service.completion.CompletionService;
import org.cyclop.test.AbstractTestCase;
import org.junit.Test;

import com.datastax.driver.core.DataType;
import com.datastax.driver.core.DataType.Name;
import com.google.common.collect.ImmutableList;
import com.google.common.testing.EqualsTester;

public class TestBeans extends AbstractTestCase {
	@Inject
	private QueryService qs;

	@Inject
	private CompletionService cs;

	@Test
	public void testSerialize_CqlDataType() throws Exception {
		CqlDataType obj = new CqlDataType(Name.ASCII, String.class, List.class, false);
		execSerializeEquals(obj, CqlDataType.class);
	}

	@Test
	public void testSerialize_CqlRowMetadata() throws Exception {
		ImmutableList.Builder<CqlExtendedColumnName> extendedColumnBuild = ImmutableList.builder();
		extendedColumnBuild.add(new CqlExtendedColumnName(CqlColumnType.COMPACT_VALUE, CqlDataType.create(DataType
				.bigint()), "col"));
		extendedColumnBuild.add(new CqlExtendedColumnName(CqlColumnType.PARTITION_KEY, CqlDataType.create(DataType
				.inet()), "col 2"));

		CqlRowMetadata obj = new CqlRowMetadata(extendedColumnBuild.build(),
				CqlPartitionKey.fromColumn(new CqlExtendedColumnName(CqlColumnType.PARTITION_KEY, CqlDataType
						.create(DataType.inet()), "col 4")));
		CqlRowMetadata des = execSerialize(obj, CqlRowMetadata.class);
		assertEquals(obj.columns, des.columns);
		assertNotNull(obj.partitionKey);
		assertNull(des.partitionKey);
	}

	@Test
	public void testSerialize_CqlColumnValue() throws Exception {
		CqlColumnValue obj = new CqlColumnValue(Long.class, new Long(11), new CqlExtendedColumnName(
				CqlColumnType.CLUSTERING_KEY, CqlDataType.TEXT, "testcol"));
		execSerializeEquals(obj, CqlColumnValue.class);
	}

	@Test
	public void testSerialize_CqlExtendedColumnName() throws Exception {
		CqlExtendedColumnName obj = new CqlExtendedColumnName(CqlColumnType.REGULAR, CqlDataType.TEXT, "testcol");
		execSerializeEquals(obj, CqlExtendedColumnName.class);
	}

	@Test
	public void testSerialize_CqlIndex() throws Exception {
		CqlIndex obj = new CqlIndex("idx1");
		execSerializeEquals(obj, CqlIndex.class);
	}

	@Test
	public void testSerialize_UserIdentifier() throws Exception {
		UserIdentifier obj = new UserIdentifier(UUID.randomUUID());
		execSerializeEquals(obj, UserIdentifier.class);
	}

	@Test
	public void testSerialize_UserPreferences() throws Exception {
		UserPreferences obj = new UserPreferences();
		obj.setShowCqlCompletionHint(true);
		execSerializeEquals(obj, UserPreferences.class);
	}

	@Test
	public void testSerialize_QueryFavourites() throws Exception {
		QueryFavourites obj = new QueryFavourites();
		obj.addWithSizeCheck(new QueryEntry(new CqlQuery(CqlQueryType.ALTER_TABLE, "alter cqldemo.mybooks ...."),
				LocalDateTime.now(), 2312));
		obj.addWithSizeCheck(new QueryEntry(new CqlQuery(CqlQueryType.ALTER_KEYSPACE,
				"alter sapce cqldemo.mybooks ...."), LocalDateTime.now(), 234));
		obj.addWithSizeCheck(new QueryEntry(new CqlQuery(CqlQueryType.SELECT, "select * from cqldemo.mybooks"),
				LocalDateTime.now(), 345));

		byte[] serialized = serialize(obj);

		QueryFavourites des = deserialize(serialized, QueryFavourites.class);
		assertTrue(des.copyAsSortedSet().containsAll(obj.copyAsSortedSet()));

		// check whether lock has been deserialized
		des.addWithSizeCheck(new QueryEntry(new CqlQuery(CqlQueryType.SELECT, "select * from cqldemo.mybooks"),
				LocalDateTime.now(), 542));
	}

	@Test
	public void testSerialize_QueryHistory() throws Exception {
		QueryHistory obj = new QueryHistory();
		obj.add(new QueryEntry(new CqlQuery(CqlQueryType.ALTER_TABLE, "alter cqldemo.mybooks ...."), LocalDateTime
				.now(), 23));
		obj.add(new QueryEntry(new CqlQuery(CqlQueryType.ALTER_KEYSPACE, "alter sapce cqldemo.mybooks ...."),
				LocalDateTime.now(), 7654));
		obj.add(new QueryEntry(new CqlQuery(CqlQueryType.SELECT, "select * from cqldemo.mybooks"), LocalDateTime.now(),
				987656));

		byte[] serialized = serialize(obj);

		QueryHistory des = deserialize(serialized, QueryHistory.class);
		assertTrue(des.copyAsList().containsAll(obj.copyAsList()));

	}

	@Test
	public void testSerialize_QueryEntry() throws Exception {
		QueryEntry obj = new QueryEntry(new CqlQuery(CqlQueryType.ALTER_TABLE, "alter cqldemo.mybooks ...."),
				LocalDateTime.now(), 1231);
		execSerializeEquals(obj, QueryEntry.class);
	}

	@Test
	public void testSerialize_CqlTable() throws Exception {
		{
			CqlTable obj = new CqlTable("mytable");
			execSerializeEquals(obj, CqlTable.class);
		}

		{
			CqlTable obj = new CqlTable("space", "mytable");
			execSerializeEquals(obj, CqlTable.class);
		}

	}

	@Test
	public void testSerialize_CqlKeySpace() throws Exception {
		CqlKeySpace obj = new CqlKeySpace("testSpace");
		execSerializeEquals(obj, CqlKeySpace.class);
	}

	@Test
	public void testSerialize_CqlKeyword() throws Exception {
		CqlKeyword obj = CqlKeyword.Def.ALLOW_FILTERING.value;
		execSerializeEquals(obj, CqlKeyword.class);
	}

	@Test
	public void testSerialize_CqlKeywordValue() throws Exception {
		CqlKeywordValue obj = CqlKeywordValue.Def.NETWORK_TOPOLOGY_STRATEGY.value;
		execSerializeEquals(obj, CqlKeywordValue.class);
	}

	@Test
	public void testSerialize_CqlNotSupported() throws Exception {
		CqlNotSupported obj = new CqlNotSupported("abx.....");
		execSerializeEquals(obj, CqlNotSupported.class);
	}

	@Test
	public void testSerialize_CqlPart() throws Exception {
		CqlPart obj = new CqlPart("abx.....");
		execSerializeEquals(obj, CqlPart.class);
	}

	@Test
	public void testSerialize_ContextCqlCompletion() throws Exception {
		ContextCqlCompletion obj = cs.findInitialCompletion();
		execSerializeEquals(obj, ContextCqlCompletion.class);
	}

	@Test
	public void testSerialize_CqlQuery() throws Exception {
		CqlQuery obj = new CqlQuery(CqlQueryType.SELECT, "select * from cqldemo.mybooks");
		execSerializeEquals(obj, CqlQuery.class);
	}

	@Test
	public void testSerialize_CqlSelectResult() throws Exception {
		CqlQueryResult res = qs.execute(new CqlQuery(CqlQueryType.SELECT, "select * from cqldemo.mybooks"));
		assertNotNull(res);
		assertNotNull(res.iterator());
		assertNotNull(res.rowMetadata.columns);
		assertTrue(res.iterator().hasNext());
		assertTrue(res.rowMetadata.columns.size() > 0);

		byte[] serBytes = serialize(res);
		CqlQueryResult des = deserialize(serBytes, CqlQueryResult.class);
		assertNotNull(des);
		assertNotNull(des.iterator());
		assertNotNull(des.rowMetadata.columns);
		assertFalse(des.iterator().hasNext());
		assertTrue(des.rowMetadata.columns.size() > 0);
		assertTrue(res.rowMetadata.columns.containsAll(des.rowMetadata.columns));
	}

	@Test
	public void testSerialize_CqlColumnName() throws Exception {
		CqlColumnName obj = new CqlColumnName(CqlDataType.TEXT, "myColumn");
		execSerializeEquals(obj, CqlColumnName.class);
	}

	@Test
	public void testSerialize_CqlPartitionKey() throws Exception {
		CqlPartitionKey obj = new CqlPartitionKey(CqlDataType.create(DataType.cfloat()), "myColumn");
		execSerializeEquals(obj, CqlPartitionKey.class);
	}

	@Test
	public void testSerialize_CqlPartitionKeyValue() throws Exception {
		CqlPartitionKeyValue obj = new CqlPartitionKeyValue(Long.class, new Long(11), new CqlPartitionKey(
				CqlDataType.TEXT, "abc"));
		execSerializeEquals(obj, CqlPartitionKeyValue.class);
	}

	@Test
	public void testEquals() {
		EqualsTester et = new EqualsTester();

		et.addEqualityGroup(new CqlColumnName(CqlDataType.TEXT, "mybooks"));
		et.addEqualityGroup(new CqlColumnName(CqlDataType.TEXT, "cqldemo"));
		et.addEqualityGroup(new CqlColumnName(CqlDataType.TEXT, "cqldemo.mybooks"));
		et.addEqualityGroup(new CqlColumnName(CqlDataType.create(DataType.ascii()), "cqldemo.mybooks"));

		et.addEqualityGroup();
		et.addEqualityGroup(new CqlExtendedColumnName(CqlColumnType.PARTITION_KEY, CqlDataType.TEXT, "cqldemo.mybooks"));
		et.addEqualityGroup(new CqlExtendedColumnName(CqlColumnType.CLUSTERING_KEY, CqlDataType.TEXT, "cqldemo.mybooks"));
		et.addEqualityGroup(new CqlExtendedColumnName(CqlColumnType.CLUSTERING_KEY,
				CqlDataType.create(DataType.ascii()), "cqldemo.mybooks"));

		et.addEqualityGroup(new CqlIndex("cqldemo"));
		et.addEqualityGroup(new CqlIndex("cqldemo.mybooks"));

		et.addEqualityGroup(new CqlKeySpace("cqldemo"));
		et.addEqualityGroup(new CqlKeySpace("cqldemo.mybooks"));

		et.addEqualityGroup(new CqlKeyword("cqldemo"));
		et.addEqualityGroup(new CqlKeyword("cqldemo.mybooks"));
		et.addEqualityGroup(CqlKeyword.Def.ALLOW_FILTERING.value);

		et.addEqualityGroup(new CqlKeywordValue("cqldemo"));
		et.addEqualityGroup(new CqlKeywordValue("cqldemo.mybooks"));
		et.addEqualityGroup(CqlKeywordValue.Def.DURABLE_WRITES.value);

		et.addEqualityGroup(new CqlPart("cqldemo"));
		et.addEqualityGroup(new CqlPart("cqldemo.mybooks"));

		et.addEqualityGroup(new CqlPartitionKey(CqlDataType.TEXT, "cqldemo.mybooks"),
				CqlPartitionKey.fromColumn(new CqlExtendedColumnName(CqlColumnType.PARTITION_KEY, CqlDataType.TEXT,
						"cqldemo.mybooks")));
		et.addEqualityGroup(new CqlPartitionKey(CqlDataType.TEXT, "cqldemo"));
		et.addEqualityGroup(new CqlPartitionKey(CqlDataType.create(DataType.counter()), "cqldemo.mybooks"));

		et.addEqualityGroup(new CqlQuery(CqlQueryType.ALTER_KEYSPACE, "cqldemo.mybooks"));
		et.addEqualityGroup(new CqlQuery(CqlQueryType.SELECT, "cqldemo.mybooks"));

		et.addEqualityGroup(new CqlTable("cqldemo", "mybooks"));
		et.addEqualityGroup(new CqlTable("cqldemo.mybooks"));
		et.addEqualityGroup(new CqlTable("cqldemo"));
		et.addEqualityGroup(new CqlTable("mybooks"));

		et.addEqualityGroup(new QueryEntry(new CqlQuery(CqlQueryType.ALTER_KEYSPACE, "cqldemo.mybooks"), 34523),
				new QueryEntry(new CqlQuery(CqlQueryType.ALTER_KEYSPACE, "cqldemo.mybooks"), LocalDateTime.now(),
						2465245));
		et.addEqualityGroup(new QueryEntry(new CqlQuery(CqlQueryType.CREATE_INDEX, "cqldemo.mybooks"), 345));
		et.addEqualityGroup(new QueryEntry(new CqlQuery(CqlQueryType.ALTER_KEYSPACE, "cqldemo"), 3452));

		et.addEqualityGroup(new UserIdentifier(UUID.randomUUID()));
		et.addEqualityGroup(new UserIdentifier(UUID.randomUUID()));
		et.addEqualityGroup(new UserIdentifier());

		UserPreferences userPreferences = new UserPreferences();
		userPreferences.setShowCqlCompletionHint(true);
		userPreferences.setShowCqlHelp(false);
		et.addEqualityGroup(userPreferences);

		userPreferences = new UserPreferences();
		userPreferences.setShowCqlCompletionHint(false);
		userPreferences.setShowCqlHelp(true);
		et.addEqualityGroup(userPreferences);

		userPreferences = new UserPreferences();
		userPreferences.setShowCqlCompletionHint(true);
		userPreferences.setShowCqlHelp(true);
		et.addEqualityGroup(new UserPreferences(), userPreferences);

		et.addEqualityGroup(new CqlColumnValue(Long.class, new Long(11), new CqlExtendedColumnName(
				CqlColumnType.CLUSTERING_KEY, CqlDataType.TEXT, "testcol")), new CqlColumnValue(Long.class, 11L,
				new CqlExtendedColumnName(CqlColumnType.CLUSTERING_KEY, CqlDataType.TEXT, "testcol")));
		et.testEquals();
	}

	private <T> T execSerializeEquals(T obj, Class<T> clazz) throws Exception {
		T ddt = execSerialize(obj, clazz);
		assertEquals(obj, ddt);
		return ddt;
	}

	private <T> T execSerialize(T obj, Class<T> clazz) throws Exception {
		byte[] serialized = serialize(obj);
		T ddt = deserialize(serialized, clazz);
		return ddt;
	}
}

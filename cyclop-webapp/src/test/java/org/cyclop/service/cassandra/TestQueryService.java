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
package org.cyclop.service.cassandra;

import static org.cyclop.model.CassandraVersion.*;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNotSame;
import static org.junit.Assert.assertTrue;

import java.util.Optional;
import java.util.UUID;

import javax.inject.Inject;

import org.cyclop.model.CassandraVersion;
import org.cyclop.model.CqlColumnName;
import org.cyclop.model.CqlColumnType;
import org.cyclop.model.CqlDataType;
import org.cyclop.model.CqlExtendedColumnName;
import org.cyclop.model.CqlIndex;
import org.cyclop.model.CqlKeySpace;
import org.cyclop.model.CqlQuery;
import org.cyclop.model.CqlQueryResult;
import org.cyclop.model.CqlQueryType;
import org.cyclop.model.CqlRowMetadata;
import org.cyclop.model.CqlTable;
import org.cyclop.model.QueryHistory;
import org.cyclop.model.exception.BeanValidationException;
import org.cyclop.model.exception.QueryException;
import org.cyclop.service.queryprotocoling.HistoryService;
import org.cyclop.test.AbstractTestCase;
import org.cyclop.test.ValidationHelper;
import org.junit.Test;

import com.datastax.driver.core.DataType;
import com.datastax.driver.core.Row;
import com.google.common.collect.ImmutableSortedSet;

/** @author Maciej Miklas */
public class TestQueryService extends AbstractTestCase {

	@Inject
	private QueryService qs;

	@Inject
	private HistoryService hs;

	@Inject
	private ValidationHelper vh;

	@Test
	public void testFindColumnNames_TableDoesNotExist() {
		ImmutableSortedSet<CqlColumnName> col = qs.findColumnNames(Optional.of(new CqlTable("not-existing")));
		vh.verifyIsEmpty(col);
	}

	@Test(expected = BeanValidationException.class)
	public void testCassandraSession_NullData() {
		cassandraSession.authenticate(null, null);
	}

	@Test
	public void testFindColumnNames_KeyspaceWithTable() {
		ImmutableSortedSet<CqlColumnName> resp = qs.findColumnNames(Optional.of(new CqlTable("cqldemo", "MyBooks")));
		assertNotNull(resp);
		assertTrue("readSize: " + resp.size(), resp.size() > 5);
		vh.verifyContainsMybooksColumns(resp, true);
		vh.verifyContainsSystemColumns(resp, false);
		vh.verifyContainsCompoundTestColumns(resp, false);
	}

	@Test(expected = BeanValidationException.class)
	public void testFindTableNames_SpaceCqlDemo_Violation_Incorrect() {
		qs.findTableNames(Optional.of(new CqlKeySpace(" ")));
	}

	@Test(expected = BeanValidationException.class)
	public void testFindTableNames_SpaceCqlDemo_Violation_Null() {
		qs.findTableNames(null);
	}

	@Test
	public void testFindTableNames_SpaceCqlDemo() {
		ImmutableSortedSet<CqlTable> col = qs.findTableNames(Optional.of(new CqlKeySpace("cqldemo")));
		vh.verifyContainsTableNamesCqlDemo(col, true);
	}

	@Test
	public void testFindTableNames_SpaceSystem() {
		ImmutableSortedSet<CqlTable> col = qs.findTableNames(Optional.of(new CqlKeySpace("system")));
		vh.verifyContainsTableNamesSystem(col, true);
	}

	@Test
	public void testFindTableNames_SpaceDoesNotExist() {
		ImmutableSortedSet<CqlTable> col = qs.findTableNames(Optional.of(new CqlKeySpace("abcx")));
		vh.verifyIsEmpty(col);
	}

	@Test
	public void testFindAllIndexes_CqlDemo() {
		ImmutableSortedSet<CqlIndex> index = qs.findAllIndexes(Optional.of(new CqlKeySpace("cqldemo")));
		vh.verifyContainsIndexFromCqlDemo(index, true);
	}

	@Test
	public void testFindAllKeySpaces() {
		ImmutableSortedSet<CqlKeySpace> kss = qs.findAllKeySpaces();
		vh.verifyContainsAllKeyspaces(kss, true);
	}

	@Test
	public void testFindAllIndexes_KeyspaceDoesNotExist() {
		ImmutableSortedSet<CqlIndex> index = qs.findAllIndexes(Optional.of(new CqlKeySpace("space...")));
		vh.verifyIsEmpty(index);
	}

	@Test(expected = BeanValidationException.class)
	public void testFindColumnNames_ViolationEmpty() {
		qs.execute(new CqlQuery(CqlQueryType.USE, " "));
	}

	@Test(expected = BeanValidationException.class)
	public void testFindColumnNames_ViolationNull() {
		qs.execute(null);
	}

	@Test
	public void testFindColumnNames_KeyspaceInSession() {
		qs.execute(new CqlQuery(CqlQueryType.USE, "use cqldemo"));
		ImmutableSortedSet<CqlColumnName> resp = qs.findColumnNames(Optional.of(new CqlTable("MyBooks")));
		assertNotNull(resp);
		assertTrue("readSize: " + resp.size(), resp.size() > 5);
		vh.verifyContainsMybooksColumns(resp, true);
		vh.verifyContainsSystemColumns(resp, false);
		vh.verifyContainsCompoundTestColumns(resp, false);
	}

	@Test
	public void testFindAllColumnNames() {
		ImmutableSortedSet<CqlColumnName> allColumnNames = qs.findAllColumnNames();
		assertNotNull(allColumnNames);
		assertFalse(allColumnNames.isEmpty());

		vh.verifyContainsMybooksColumns(allColumnNames, true);
		vh.verifyContainsSystemColumns(allColumnNames, true);
		if(cassandraSession.getCassandraVersion().after(VER_1_2)) {
			vh.verifyContainsCompoundTestColumns(allColumnNames, true);
		}
	}

	@Test
	public void testExecute_CompoundPkNoDynamicColumns() {
		qs.execute(new CqlQuery(CqlQueryType.USE, "USE CqlDemo"));
		CqlQuery query = new CqlQuery(CqlQueryType.SELECT, "select * from CompoundTest where deesc='TEST_SET_1'");
		try (QueryHistory.HistoryIterator iterator = hs.read().iterator()) {
			assertNotSame(query, iterator.next().query);
		}
		CqlQueryResult res = qs.execute(query);
		try (QueryHistory.HistoryIterator iterator = hs.read().iterator()) {
			assertEquals(query, iterator.next().query);
		}

		CqlRowMetadata rowMetadata = res.rowMetadata;
		assertEquals(4, rowMetadata.columns.size());

		String comColsStr = rowMetadata.columns.toString();
		assertTrue(comColsStr, rowMetadata.columns.contains(
				new CqlExtendedColumnName(CqlColumnType.PARTITION_KEY, CqlDataType.create(DataType.uuid()), "id")));

		if(cassandraSession.getCassandraVersion().after(VER_1_2)) {

			assertTrue(comColsStr, rowMetadata.columns.contains(
					new CqlExtendedColumnName(CqlColumnType.CLUSTERING_KEY, CqlDataType.create(DataType.cint()), "id2")));

			assertTrue(comColsStr, rowMetadata.columns.contains(
					new CqlExtendedColumnName(CqlColumnType.CLUSTERING_KEY, CqlDataType.create(DataType.varchar()),
							"id3")));
		}else{
			// TODO Cassandra 1.2 does not recognize clustering key
			assertTrue(comColsStr, rowMetadata.columns.contains(
					new CqlExtendedColumnName(CqlColumnType.REGULAR, CqlDataType.create(DataType.cint()), "id2")));

			assertTrue(comColsStr, rowMetadata.columns.contains(
					new CqlExtendedColumnName(CqlColumnType.REGULAR, CqlDataType.create(DataType.varchar()),
							"id3")));
		}

		assertTrue(comColsStr, rowMetadata.columns.contains(new CqlExtendedColumnName(CqlColumnType.REGULAR,
				CqlDataType.create(DataType.varchar()), "deesc")));

		int rowsCnt = 0;
		for (Row row : res) {
			rowsCnt++;
			assertEquals("TEST_SET_1", row.getString("deesc"));
		}
		assertEquals(50, rowsCnt);
	}

	@Test(expected = BeanValidationException.class)
	public void testExecute_IncorrectParams() {
		qs.execute(new CqlQuery(null, null));
	}

	@Test(expected = BeanValidationException.class)
	public void testExecuteSimple_IncorrectParams() {
		qs.executeSimple(new CqlQuery(null, null), false);
	}

	@Test(expected = QueryException.class)
	public void testExecuteSimple_QueryError() {
		qs.executeSimple(new CqlQuery(CqlQueryType.SELECT, "select * from bara.bara"), false);
	}

	@Test
	public void testExecuteSimple_Select_UpdateHistory() {
		executeSimpleSelect(true);
	}

	@Test
	public void testExecuteSimple_Select_DoNotUpdateHistory() {
		executeSimpleSelect(false);
	}

	private void executeSimpleSelect(boolean updateHistory) {
		CqlQuery testCql = new CqlQuery(CqlQueryType.SELECT, "select title from cqldemo.mybooks where id="
				+ UUID.randomUUID());

		try (QueryHistory.HistoryIterator iterator1 = hs.read().iterator()) {
			if (iterator1.hasNext()) {
				assertNotSame(testCql, iterator1.next().query);
			}
		}
		qs.executeSimple(testCql, updateHistory);
		if (updateHistory) {
			try (QueryHistory.HistoryIterator iterator = hs.read().iterator()) {
				assertEquals(testCql, iterator.next().query);
			}
		} else {
			try (QueryHistory.HistoryIterator iterator = hs.read().iterator()) {
				if (iterator.hasNext()) {
					assertNotSame(testCql, iterator.next().query);
				}
			}
		}

		assertFalse(qs.execute(testCql).iterator().hasNext());
		try (QueryHistory.HistoryIterator iterator = hs.read().iterator()) {
			assertEquals(testCql, iterator.next().query);
		}
	}

	@Test
	public void testExecuteSimple_Insert() {

		CqlQuery testCql = new CqlQuery(CqlQueryType.SELECT,
				"select title from cqldemo.mybooks where id=d0302001-bd93-42a2-8bc8-79bbdbfc7717");
		assertFalse(qs.execute(testCql).iterator().hasNext());

		qs.executeSimple(new CqlQuery(CqlQueryType.INSERT,
				"INSERT INTO CqlDemo.MyBooks (id,title) VALUES (d0302001-bd93-42a2-8bc8-79bbdbfc7717,'some value')"),
				false);

		assertTrue(qs.execute(testCql).iterator().hasNext());
	}

	@Test(expected = QueryException.class)
	public void testExecute_QueryError() {
		qs.execute(new CqlQuery(CqlQueryType.SELECT, "select * from bara.bara"), false);
	}

	@Test
	public void testExecute_SimplePkWithDynamicColumn() {
		qs.execute(new CqlQuery(CqlQueryType.USE, "USE CqlDemo"));
		CqlQueryResult res = qs.execute(new CqlQuery(CqlQueryType.SELECT, "select * from MyBooks where pages=2212"));

		CqlRowMetadata rowMetadata = res.rowMetadata;
		assertTrue(res.toString(), rowMetadata.columns.contains(new CqlExtendedColumnName(CqlColumnType.REGULAR,
				CqlDataType.create(DataType.varchar()), "title")));

		String comColsStr = rowMetadata.columns.toString();
		assertTrue(comColsStr, rowMetadata.columns.contains(new CqlExtendedColumnName(CqlColumnType.PARTITION_KEY,
				CqlDataType.create(DataType.uuid()), "id")));

		assertTrue(comColsStr, rowMetadata.columns.contains(new CqlExtendedColumnName(CqlColumnType.REGULAR,
				CqlDataType.create(DataType.set(DataType.varchar())), "authors")));

		assertTrue(comColsStr, rowMetadata.columns.contains(new CqlExtendedColumnName(CqlColumnType.REGULAR,
				CqlDataType.create(DataType.cint()), "pages")));

		assertTrue(comColsStr, rowMetadata.columns.contains(new CqlExtendedColumnName(CqlColumnType.REGULAR,
				CqlDataType.create(DataType.map(DataType.varchar(), DataType.cdouble())), "price")));

		int rowsSize = 0;
		for (Row row : res) {
			rowsSize++;
			int idx = row.getInt("idx");
			assertEquals("Midnight Rain-" + idx, row.getString("title"));
		}

		assertEquals(100, rowsSize);
	}

	@Test
	public void testCheckTableExists_NoKeyspce() {
		assertFalse(qs.checkTableExists(new CqlTable("bra")));
		assertFalse(qs.checkTableExists(new CqlTable("CqlDemo")));
		assertFalse(qs.checkTableExists(new CqlTable("sd dd ")));
		assertFalse(qs.checkTableExists(new CqlTable("CQLDEMO MYBOOKS")));
		assertFalse(qs.checkTableExists(new CqlTable("CqlDemo.MyBooks")));
		assertFalse(qs.checkTableExists(new CqlTable("CqlDemo.MyBooks ")));
		assertFalse(qs.checkTableExists(new CqlTable("  CqlDemo.MyBooks ")));
		assertFalse(qs.checkTableExists(new CqlTable("CqlDEmo.MyBOoks ")));
		assertFalse(qs.checkTableExists(new CqlTable("cqldemo.mybooks")));
		assertFalse(qs.checkTableExists(new CqlTable("CQLDEMO.MYBOOKS")));

		assertTrue(qs.checkTableExists(new CqlTable("MYBOOKS")));
		assertTrue(qs.checkTableExists(new CqlTable("MYBoOKS")));
		assertTrue(qs.checkTableExists(new CqlTable("mybooks")));
		assertTrue(qs.checkTableExists(new CqlTable("asd", "mybooks")));
	}
}

package org.cyclop.service.cassandra;

import com.datastax.driver.core.DataType;
import com.datastax.driver.core.Row;
import com.google.common.collect.ImmutableSortedSet;
import org.cyclop.model.CqlColumnName;
import org.cyclop.model.CqlColumnType;
import org.cyclop.model.CqlDataType;
import org.cyclop.model.CqlExtendedColumnName;
import org.cyclop.model.CqlIndex;
import org.cyclop.model.CqlKeySpace;
import org.cyclop.model.CqlQuery;
import org.cyclop.model.CqlQueryName;
import org.cyclop.model.CqlSelectResult;
import org.cyclop.model.CqlTable;
import org.cyclop.model.exception.BeanValidationException;
import org.cyclop.test.AbstractTestCase;
import org.cyclop.test.ValidationHelper;
import org.junit.Test;

import javax.inject.Inject;

import static junit.framework.Assert.assertNotNull;
import static junit.framework.Assert.assertTrue;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;

/** @author Maciej Miklas */
public class TestQueryService extends AbstractTestCase {

	@Inject
	private QueryService qs;

	@Inject
	private ValidationHelper vh;

	@Test
	public void testFindColumnNames_TableDoesNotExist() {
		ImmutableSortedSet<CqlColumnName> col = qs.findColumnNames(new CqlTable("not-existing"));
		vh.verifyIsEmpty(col);
	}

	@Test(expected = BeanValidationException.class)
	public void testCassandraSession_NullData() {
		cassandraSession.authenticate(null, null);
	}

	@Test
	public void testFindColumnNames_KeyspaceWithTable() {
		ImmutableSortedSet<CqlColumnName> resp = qs.findColumnNames(new CqlTable("cqldemo", "MyBooks"));
		assertNotNull(resp);
		assertTrue("size: " + resp.size(), resp.size() > 5);
		vh.verifyContainsMybooksColumns(resp, true);
		vh.verifyContainsSystemColumns(resp, false);
		vh.verifyContainsCompoundTestColumns(resp, false);
	}

	@Test(expected = BeanValidationException.class)
	public void testFindTableNames_SpaceCqlDemo_Violation() {
		qs.findTableNames(new CqlKeySpace(" "));
	}

	@Test
	public void testFindTableNames_SpaceCqlDemo() {
		ImmutableSortedSet<CqlTable> col = qs.findTableNames(new CqlKeySpace("cqldemo"));
		vh.verifyContainsTableNamesCqlDemo(col, true);
	}

	// TODO why does it contain NodeInfo - it's not valid table
	// TODO add "select * from xxx" test case on returned tables
	@Test
	public void testFindTableNames_SpaceSystem() {
		ImmutableSortedSet<CqlTable> col = qs.findTableNames(new CqlKeySpace("system"));
		vh.verifyContainsTableNamesSystem(col, true);
	}

	@Test
	public void testFindTableNames_SpaceDoesNotExist() {
		ImmutableSortedSet<CqlTable> col = qs.findTableNames(new CqlKeySpace("abcx"));
		vh.verifyIsEmpty(col);
	}

	@Test
	public void testFindAllIndexes_CqlDemo() {
		ImmutableSortedSet<CqlIndex> index = qs.findAllIndexes(new CqlKeySpace("cqldemo"));
		vh.verifyContainsIndexFromCqlDemo(index, true);
	}

	@Test
	public void testFindAllKeySpaces() {
		ImmutableSortedSet<CqlKeySpace> kss = qs.findAllKeySpaces();
		vh.verifyContainsAllKeyspaces(kss, true);
	}

	@Test
	public void testFindAllIndexes_KeyspaceDoesNotExist() {
		ImmutableSortedSet<CqlIndex> index = qs.findAllIndexes(new CqlKeySpace("space..."));
		vh.verifyIsEmpty(index);
	}

	@Test(expected = BeanValidationException.class)
	public void testFindColumnNames_ViolationEmpty() {
		qs.execute(new CqlQuery(CqlQueryName.USE, " "));
	}

	@Test(expected = BeanValidationException.class)
	public void testFindColumnNames_ViolationNull() {
		qs.execute(null);
	}

	@Test
	public void testFindColumnNames_KeyspaceInSession() {
		qs.execute(new CqlQuery(CqlQueryName.USE, "use cqldemo"));
		ImmutableSortedSet<CqlColumnName> resp = qs.findColumnNames(new CqlTable("MyBooks"));
		assertNotNull(resp);
		assertTrue("size: " + resp.size(), resp.size() > 5);
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
		vh.verifyContainsCompoundTestColumns(allColumnNames, true);
	}

	@Test
	public void testExecute_CompoundPkNoDynamicColumns() {
		qs.execute(new CqlQuery(CqlQueryName.USE, "USE CqlDemo"));
		CqlSelectResult res = qs
				.execute(new CqlQuery(CqlQueryName.SELECT, "select * from CompoundTest where deesc='TEST_SET_1'"));
		assertEquals(50, res.rows.size());

		assertEquals(4, res.commonColumns.size());
		assertEquals(0, res.dynamicColumns.size());

		String comColsStr = res.commonColumns.toString();
		assertTrue(comColsStr, res.commonColumns.contains(
				new CqlExtendedColumnName(CqlColumnType.PARTITION_KEY, CqlDataType.create(DataType.uuid()), "id")));

		assertTrue(comColsStr, res.commonColumns.contains(
				new CqlExtendedColumnName(CqlColumnType.CLUSTERING_KEY, CqlDataType.create(DataType.cint()), "id2")));

		assertTrue(comColsStr, res.commonColumns.contains(
				new CqlExtendedColumnName(CqlColumnType.CLUSTERING_KEY, CqlDataType.create(DataType.varchar()),
						"id3")));

		assertTrue(comColsStr, res.commonColumns.contains(
				new CqlExtendedColumnName(CqlColumnType.REGULAR, CqlDataType.create(DataType.varchar()), "deesc")));

		for (Row row : res.rows) {
			assertEquals("TEST_SET_1", row.getString("deesc"));
		}
	}

	@Test(expected = BeanValidationException.class)
	public void testExecute_Violation() {
		qs.execute(new CqlQuery(null, null));
	}

	@Test
	public void testExecute_SimplePkWithDynamicColumn() {
		qs.execute(new CqlQuery(CqlQueryName.USE, "USE CqlDemo"));
		CqlSelectResult res = qs.execute(new CqlQuery(CqlQueryName.SELECT, "select * from MyBooks where pages=2212"));
		assertEquals(100, res.rows.size());

		assertTrue(res.toString(), res.dynamicColumns.contains(
				new CqlExtendedColumnName(CqlColumnType.REGULAR, CqlDataType.create(DataType.varchar()), "genre")));

		String comColsStr = res.commonColumns.toString();
		assertTrue(comColsStr, res.commonColumns.contains(
				new CqlExtendedColumnName(CqlColumnType.PARTITION_KEY, CqlDataType.create(DataType.uuid()), "id")));

		assertTrue(comColsStr, res.commonColumns.contains(
				new CqlExtendedColumnName(CqlColumnType.REGULAR, CqlDataType.create(DataType.set(DataType.varchar())),
						"authors")));

		assertTrue(comColsStr, res.commonColumns.contains(
				new CqlExtendedColumnName(CqlColumnType.REGULAR, CqlDataType.create(DataType.cint()), "pages")));

		assertTrue(comColsStr, res.commonColumns.contains(new CqlExtendedColumnName(CqlColumnType.REGULAR,
				CqlDataType.create(DataType.map(DataType.varchar(), DataType.cdouble())), "price")));

		for (Row row : res.rows) {
			int idx = row.getInt("idx");
			assertEquals("Midnight Rain-" + idx, row.getString("title"));
		}

	}
}

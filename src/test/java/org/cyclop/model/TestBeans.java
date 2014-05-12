package org.cyclop.model;

import com.datastax.driver.core.DataType;
import com.datastax.driver.core.DataType.Name;
import com.google.common.testing.EqualsTester;
import org.cyclop.service.cassandra.QueryService;
import org.cyclop.service.completion.CompletionService;
import org.cyclop.test.AbstractTestCase;
import org.joda.time.DateTime;
import org.junit.Test;

import javax.inject.Inject;
import java.util.List;
import java.util.UUID;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

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
	public void testSerialize_CqlColumnValue() throws Exception {
		CqlColumnValue obj = new CqlColumnValue(Long.class, new Long(11),
				new CqlExtendedColumnName(CqlColumnType.CLUSTERING_KEY, CqlDataType.TEXT, "testcol"));
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
		obj.addWithSizeCheck(
				new QueryEntry(new CqlQuery(CqlQueryType.ALTER_TABLE, "alter cqldemo.mybooks ...."), DateTime.now(),
						2312, 34));
		obj.addWithSizeCheck(
				new QueryEntry(new CqlQuery(CqlQueryType.ALTER_KEYSPACE, "alter sapce cqldemo.mybooks ...."),
						DateTime.now(), 234, 34));
		obj.addWithSizeCheck(
				new QueryEntry(new CqlQuery(CqlQueryType.SELECT, "select * from cqldemo.mybooks"), DateTime.now(), 345,
						34));

		byte[] serialized = serialize(obj);

		QueryFavourites des = deserialize(serialized, QueryFavourites.class);
		assertTrue(des.copyAsSortedSet().containsAll(obj.copyAsSortedSet()));

		// check whether lock has been deserialized
		des.addWithSizeCheck(
				new QueryEntry(new CqlQuery(CqlQueryType.SELECT, "select * from cqldemo.mybooks"), DateTime.now(), 542,
						34));
	}

	@Test
	public void testSerialize_QueryHistory() throws Exception {
		QueryHistory obj = new QueryHistory();
		obj.add(new QueryEntry(new CqlQuery(CqlQueryType.ALTER_TABLE, "alter cqldemo.mybooks ...."), DateTime.now(), 23,
				34));
		obj.add(new QueryEntry(new CqlQuery(CqlQueryType.ALTER_KEYSPACE, "alter sapce cqldemo.mybooks ...."),
				DateTime.now(), 7654, 34));
		obj.add(new QueryEntry(new CqlQuery(CqlQueryType.SELECT, "select * from cqldemo.mybooks"), DateTime.now(),
				987656, 34));

		byte[] serialized = serialize(obj);

		QueryHistory des = deserialize(serialized, QueryHistory.class);
		assertTrue(des.copyAsList().containsAll(obj.copyAsList()));

	}


	@Test
	public void testSerialize_QueryEntry() throws Exception {
		QueryEntry obj = new QueryEntry(new CqlQuery(CqlQueryType.ALTER_TABLE, "alter cqldemo.mybooks ...."),
				DateTime.now(), 1231, 34);
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
		assertNotNull(res.commonColumns);
		assertNotNull(res.dynamicColumns);
		assertTrue(res.rowsSize > 0);
		assertTrue(res.commonColumns.size() > 0);

		byte[] serBytes = serialize(res);
		CqlQueryResult des = deserialize(serBytes, CqlQueryResult.class);
		assertNotNull(des);
		assertNotNull(des.iterator());
		assertNotNull(des.commonColumns);
		assertNotNull(des.dynamicColumns);
		assertEquals(0, des.rowsSize);
		assertTrue(des.commonColumns.size() > 0);
		assertTrue(res.commonColumns.containsAll(des.commonColumns));
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
		CqlPartitionKeyValue obj = new CqlPartitionKeyValue(Long.class, new Long(11),
				new CqlPartitionKey(CqlDataType.TEXT, "abc"));
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
		et.addEqualityGroup(
				new CqlExtendedColumnName(CqlColumnType.PARTITION_KEY, CqlDataType.TEXT, "cqldemo.mybooks"));
		et.addEqualityGroup(
				new CqlExtendedColumnName(CqlColumnType.CLUSTERING_KEY, CqlDataType.TEXT, "cqldemo.mybooks"));
		et.addEqualityGroup(
				new CqlExtendedColumnName(CqlColumnType.CLUSTERING_KEY, CqlDataType.create(DataType.ascii()),
						"cqldemo.mybooks"));

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

		et.addEqualityGroup(new CqlPartitionKey(CqlDataType.TEXT, "cqldemo.mybooks"), CqlPartitionKey.fromColumn(
				new CqlExtendedColumnName(CqlColumnType.PARTITION_KEY, CqlDataType.TEXT, "cqldemo.mybooks")));
		et.addEqualityGroup(new CqlPartitionKey(CqlDataType.TEXT, "cqldemo"));
		et.addEqualityGroup(new CqlPartitionKey(CqlDataType.create(DataType.counter()), "cqldemo.mybooks"));

		et.addEqualityGroup(new CqlQuery(CqlQueryType.ALTER_KEYSPACE, "cqldemo.mybooks"));
		et.addEqualityGroup(new CqlQuery(CqlQueryType.SELECT, "cqldemo.mybooks"));

		et.addEqualityGroup(new CqlTable("cqldemo", "mybooks"));
		et.addEqualityGroup(new CqlTable("cqldemo.mybooks"));
		et.addEqualityGroup(new CqlTable("cqldemo"));
		et.addEqualityGroup(new CqlTable("mybooks"));

		et.addEqualityGroup(new QueryEntry(new CqlQuery(CqlQueryType.ALTER_KEYSPACE, "cqldemo.mybooks"), 34523, 34),
				new QueryEntry(new CqlQuery(CqlQueryType.ALTER_KEYSPACE, "cqldemo.mybooks"), new DateTime(), 2465245,
						34));
		et.addEqualityGroup(new QueryEntry(new CqlQuery(CqlQueryType.CREATE_INDEX, "cqldemo.mybooks"), 345, 34));
		et.addEqualityGroup(new QueryEntry(new CqlQuery(CqlQueryType.ALTER_KEYSPACE, "cqldemo"), 3452, 34));

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

		et.addEqualityGroup(new CqlColumnValue(Long.class, new Long(11),
				new CqlExtendedColumnName(CqlColumnType.CLUSTERING_KEY, CqlDataType.TEXT, "testcol")),
				new CqlColumnValue(Long.class, 11L,
						new CqlExtendedColumnName(CqlColumnType.CLUSTERING_KEY, CqlDataType.TEXT, "testcol")));
		et.testEquals();
	}

	private <T> T execSerializeEquals(T obj, Class<T> clazz) throws Exception {
		byte[] serialized = serialize(obj);

		T ddt = deserialize(serialized, clazz);
		assertEquals(obj, ddt);
		return ddt;
	}
}

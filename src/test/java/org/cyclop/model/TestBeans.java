package org.cyclop.model;

import com.datastax.driver.core.DataType;
import com.google.common.testing.EqualsTester;
import org.cyclop.service.cassandra.QueryService;
import org.cyclop.test.AbstractTestCase;
import org.joda.time.DateTime;
import org.junit.Test;

import javax.inject.Inject;
import java.io.NotSerializableException;
import java.util.UUID;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

public class TestBeans extends AbstractTestCase {
	@Inject
	private QueryService qs;

	@Test(expected = NotSerializableException.class)
	public void testCqlSelectResult_Serialize() throws Exception {
		CqlSelectResult res = qs.execute(new CqlQuery(CqlQueryName.SELECT, "select * from cqldemo.mybooks"));
		assertNotNull(res);
		assertNotNull(res.rows);
		assertNotNull(res.commonColumns);
		assertNotNull(res.dynamicColumns);
		assertTrue(res.rows.size() > 0);

		serialize(res);
	}

	@Test(expected = NotSerializableException.class)
	public void testCqlColumnName_Serialize() throws Exception {
		serialize(new CqlColumnName(DataType.text(), "myColumn"));
	}

	@Test(expected = NotSerializableException.class)
	public void testCqlPartitionKey_Serialize() throws Exception {
		serialize(new CqlPartitionKey(DataType.text(), "myColumn"));
	}

	@Test
	public void testEquals() {
		EqualsTester et = new EqualsTester();

		et.addEqualityGroup(new CqlColumnName(DataType.text(), "mybooks"));
		et.addEqualityGroup(new CqlColumnName(DataType.text(), "cqldemo"));
		et.addEqualityGroup(new CqlColumnName(DataType.text(), "cqldemo.mybooks"));
		et.addEqualityGroup(new CqlColumnName(DataType.ascii(), "cqldemo.mybooks"));

		et.addEqualityGroup();
		et.addEqualityGroup(new CqlExtendedColumnName(CqlColumnType.PARTITION_KEY, DataType.text(), "cqldemo.mybooks"));
		et.addEqualityGroup(
				new CqlExtendedColumnName(CqlColumnType.CLUSTERING_KEY, DataType.text(), "cqldemo.mybooks"));
		et.addEqualityGroup(
				new CqlExtendedColumnName(CqlColumnType.CLUSTERING_KEY, DataType.ascii(), "cqldemo.mybooks"));

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

		et.addEqualityGroup(new CqlPartitionKey(DataType.text(), "cqldemo.mybooks"), CqlPartitionKey.fromColumn(
				new CqlExtendedColumnName(CqlColumnType.PARTITION_KEY, DataType.text(), "cqldemo.mybooks")));
		et.addEqualityGroup(new CqlPartitionKey(DataType.text(), "cqldemo"));
		et.addEqualityGroup(new CqlPartitionKey(DataType.counter(), "cqldemo.mybooks"));

		et.addEqualityGroup(new CqlQuery(CqlQueryName.ALTER_KEYSPACE, "cqldemo.mybooks"));
		et.addEqualityGroup(new CqlQuery(CqlQueryName.SELECT, "cqldemo.mybooks"));

		et.addEqualityGroup(new CqlTable("cqldemo", "mybooks"));
		et.addEqualityGroup(new CqlTable("cqldemo.mybooks"));
		et.addEqualityGroup(new CqlTable("cqldemo"));
		et.addEqualityGroup(new CqlTable("mybooks"));

		et.addEqualityGroup(new QueryEntry(new CqlQuery(CqlQueryName.ALTER_KEYSPACE, "cqldemo.mybooks")),
				new QueryEntry(new CqlQuery(CqlQueryName.ALTER_KEYSPACE, "cqldemo.mybooks"), new DateTime()));
		et.addEqualityGroup(new QueryEntry(new CqlQuery(CqlQueryName.CREATE_INDEX, "cqldemo.mybooks")));
		et.addEqualityGroup(new QueryEntry(new CqlQuery(CqlQueryName.ALTER_KEYSPACE, "cqldemo")));

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
		et.testEquals();
	}
}

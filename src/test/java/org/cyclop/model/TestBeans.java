package org.cyclop.model;

import com.datastax.driver.core.DataType;
import com.google.common.testing.EqualsTester;
import org.joda.time.DateTime;
import org.junit.Test;

import java.util.UUID;

public class TestBeans {

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

		et.addEqualityGroup(new QueryHistoryEntry(new CqlQuery(CqlQueryName.ALTER_KEYSPACE, "cqldemo.mybooks")),
				new QueryHistoryEntry(new CqlQuery(CqlQueryName.ALTER_KEYSPACE, "cqldemo.mybooks"), new DateTime()));
		et.addEqualityGroup(new QueryHistoryEntry(new CqlQuery(CqlQueryName.CREATE_INDEX, "cqldemo.mybooks")));
		et.addEqualityGroup(new QueryHistoryEntry(new CqlQuery(CqlQueryName.ALTER_KEYSPACE, "cqldemo")));

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

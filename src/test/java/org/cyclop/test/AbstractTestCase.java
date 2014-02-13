package org.cyclop.test;

import com.datastax.driver.core.Session;
import org.apache.cassandra.io.util.FileUtils;
import org.cyclop.model.CqlQuery;
import org.cyclop.model.CqlQueryName;
import org.cyclop.service.cassandra.CassandraSession;
import org.cyclop.service.cassandra.QueryService;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.runner.RunWith;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import org.springframework.test.context.web.WebAppConfiguration;

import javax.inject.Inject;
import java.io.File;
import java.io.IOException;
import java.nio.file.FileSystems;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.UUID;

import static org.junit.Assert.assertNotNull;

/** @author Maciej Miklas */
@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(locations = {TestProps.TEST_CONTEXT})
@WebAppConfiguration
public abstract class AbstractTestCase {

	private static final EmbeddedCassandra CASSANDRA = new EmbeddedCassandra();

	private static boolean INIT_EXECUTED = false;

	private static boolean INIT_QUERY_EXECUTED = false;

	protected final boolean unixOs = !System.getProperty("os.name", "linux").toLowerCase().contains("windows");

	@Inject
	private QueryService qs;

	@Inject
	protected CassandraSession cassandraSession;

	@Before
	public void setup() {
		cassandraSession.authenticate("test", "test1234");
		createTestData();
	}

	private void createTestData() {
		if (INIT_QUERY_EXECUTED) {
			return;
		}
		INIT_QUERY_EXECUTED = true;
		qs.execute(new CqlQuery(CqlQueryName.USE, "USE CqlDemo"));

		// all tables have this common value: deesc=TEST_SET_1
		{
			for (int i = 0; i < 50; i++) {
				StringBuilder cql = new StringBuilder("INSERT INTO CompoundTest (id,id2,id3,deesc) VALUES (");
				cql.append(UUID.randomUUID()).append(",");
				cql.append(i);
				cql.append(",'abc','TEST_SET_1')");
				qs.execute(new CqlQuery(CqlQueryName.INSERT, cql.toString()));
			}
		}

		// all tables have this common value: pages = 2212
		{
			for (int i = 0; i < 100; i++) {
				StringBuilder cql = new StringBuilder(
						"INSERT INTO MyBooks (id,title,genre,publishDate,description,authors,pages,price,idx) VALUES (");
				cql.append(UUID.randomUUID()).append(",");
				cql.append("'Midnight Rain-").append(i).append("',");
				if (i == 0) {
					cql.append("'Fantasy',");
				} else {
					cql.append("null,");
				}
				cql.append("'2000-10-01',");

				cql.append("'Description.....-").append(i).append("',");
				cql.append("{'Ralls, Kim'},2212, {'D':2.85,'E':3.11,'F':4.22},");
				cql.append(i).append(")");
				qs.execute(new CqlQuery(CqlQueryName.INSERT, cql.toString()));
			}
		}
	}

	@BeforeClass
	public static void staticInit() throws Exception {
		if (INIT_EXECUTED) {
			return;
		}
		INIT_EXECUTED = true;
		setupHistory();
		setupCassandra();
	}

	private static void setupHistory() throws Exception {
		Path tempPath = FileSystems.getDefault().getPath("target", "cyclop-history-test");
		rmdir(tempPath);
		Files.createDirectory(tempPath);
		System.getProperties().setProperty("history.folder", tempPath.toString());
	}

	private static void setupCassandra() throws Exception {
		CASSANDRA.start();
	}

	private static void rmdir(Path dir) throws IOException {
		File dirFile = dir.toFile();
		if (dirFile.exists()) {
			FileUtils.deleteRecursive(dirFile);
		}
	}

	public Session getCassandraSession() {
		assertNotNull("Cassandra session is null", CASSANDRA.getSession());
		return CASSANDRA.getSession();
	}
}

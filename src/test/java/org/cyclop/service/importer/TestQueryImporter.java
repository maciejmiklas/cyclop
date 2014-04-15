package org.cyclop.service.importer;

import org.cyclop.model.CqlQuery;
import org.cyclop.model.CqlQueryType;
import org.cyclop.model.exception.QueryException;
import org.cyclop.service.cassandra.QueryService;
import org.cyclop.service.importer.model.ImportStats;
import org.cyclop.test.AbstractTestCase;
import org.junit.Test;

import javax.inject.Inject;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

public class TestQueryImporter extends AbstractTestCase {

	@Inject
	private QueryImporter importer;

	@Inject
	private QueryService queryService;

	@Test
	public void testImportOneQueryPerLine() throws Exception {
		try (InputStream fio = getClass().getResourceAsStream("/createDemoData.cql")) {
			ResultConsumer rc = new ResultConsumer();
			ImportStats stats = importer.importScript(fio, rc, true, true);
			assertEquals(6, rc.size());
			assertEquals(0, rc.error.size());
			assertEquals(6, rc.success.size());
			assertEquals(0, stats.errorCount);
			assertEquals(6, stats.successCount);
			assertTrue(rc.toString(), rc.success.contains(new CqlQuery(CqlQueryType.UNKNOWN,
					"INSERT INTO MyBooks (id,title,pages,price) VALUES (0f6939a7-62f7-4ed0-a909-6fc302764c8d,'just title.....',112291,{'DE':4,'EU':34})")));
		}
	}

	@Test
	public void testImportMix() throws Exception {
		try (InputStream fio = getClass().getResourceAsStream("/createDemoDataMix.cql")) {
			ResultConsumer rc = new ResultConsumer();
			ImportStats stats = importer.importScript(fio, rc, true, true);
			assertEquals(8, rc.size());
			assertEquals(3, rc.error.size());
			assertEquals(5, rc.success.size());
			assertEquals(3, stats.errorCount);
			assertEquals(5, stats.successCount);
			assertTrue(rc.toString(), rc.success.contains(new CqlQuery(CqlQueryType.UNKNOWN, "USE CqlDemo")));
			assertTrue(rc.toString(), rc.success.contains(new CqlQuery(CqlQueryType.UNKNOWN,
					"INSERT INTO MyBooks (id,title,genre,publishDate,description,authors,pages,price,paperType) VALUES (1ff18f41-cfb8-45ff-9e89-fb20f95ffc5d,'XML Developers Guide','Computer','2000-10-01','An in-depth look at creating applications with XML.',{'Gambardella, Matthew','Marcin Miklas','Fryderyk Zajac','Anna Zajac'},112291,{'D':3.85,'E':4.11,'F':4.00},'white and soft')")));
			assertTrue(rc.toString(), rc.success.contains(new CqlQuery(CqlQueryType.UNKNOWN,
					"INSERT INTO MyBooks (id,title,pages,price) VALUES (44f2054c-f98b-43a7-833d-0e1358fdee82,'just title.....',112291,{'DE':4,'EU':34})")));
			assertTrue(rc.toString(), rc.success.contains(new CqlQuery(CqlQueryType.UNKNOWN,
					"INSERT INTO MyBooks (id,title,pages,price) VALUES (c746c90c-94dc-45dc-9b47-e410e46a0e61,'just title..... urrr...',112291,{'DE':44,'EU':343})")));
			assertTrue(rc.toString(), rc.success.contains(new CqlQuery(CqlQueryType.UNKNOWN,
					"INSERT INTO MyBooks (id,title,pages,            price) VALUES (e1390b2e-1393-490b-aa6e-88874ac1fc88,            'just title. wed dwe....',112291,           {'DE':4,'EU':324})")));

			{
				QueryException res = rc.error
						.get(new CqlQuery(CqlQueryType.UNKNOWN, "asdf adf adfa;sdf as'fasdf;asdf ;;q34t24gtvrf"));
				assertNotNull(rc.toString(), res);
				assertTrue(res.toString(), res.getMessage().contains("Error executing CQL"));
				assertTrue(res.toString(),
						res.getMessage().contains("reason: line 1:45 mismatched character '<EOF>' expecting"));
			}

			{
				QueryException res = rc.error.get(new CqlQuery(CqlQueryType.UNKNOWN,
						"INSERT INTO MyBooks (id,title,pages,price) VALUE (38710416-6253-4a77-a31c-6429f16f3837,'just title..... NR 2',112291,{'DE':4,'EU':4})"));
				assertNotNull(rc.toString(), res);
				assertTrue(res.toString(), res.getMessage().contains("Error executing CQL"));
				assertTrue(res.toString(),
						res.getMessage().contains("reason: line 1:43 mismatched input 'VALUE' expecting K_VALUES"));
			}

			{
				QueryException res = rc.error.get(new CqlQuery(CqlQueryType.UNKNOWN,
						"INSERT INTO MyBooks (id,title,pages,price) VALUES (6ceb1c47-0955-4654-80d4-5230b88467d2,'just title.....',112291)"));
				assertNotNull(rc.toString(), res);
				assertTrue(res.toString(), res.getMessage().contains("Error executing CQL"));
				assertTrue(res.toString(), res.getMessage().contains("Unmatched column names/values"));
			}

		}
	}

	@Test
	public void testImportLineBreaks() throws Exception {
		CqlQuery testCql = new CqlQuery(CqlQueryType.SELECT,
				"select title from cqldemo.mybooks where id=644556a9-651b-45b3-bd01-cc807c64bd4f");
		assertTrue(queryService.execute(testCql).isEmpty());

		try (InputStream fio = getClass().getResourceAsStream("/createDemoDataLb.cql")) {
			ResultConsumer rc = new ResultConsumer();
			ImportStats stats = importer.importScript(fio, rc, true, true);
			assertEquals(8, rc.size());
			assertEquals(0, rc.error.size());
			assertEquals(8, rc.success.size());
			assertEquals(0, stats.errorCount);
			assertEquals(8, stats.successCount);
			assertTrue(rc.toString(), rc.success.contains(new CqlQuery(CqlQueryType.UNKNOWN, "USE CqlDemo")));

			assertTrue(rc.toString(), rc.success.contains(new CqlQuery(CqlQueryType.UNKNOWN,
					"INSERT INTO MyBooks (id,title,genre,publishDate,description,authors,pages,price,paperType) VALUES (1ff18f41-cfb8-45ff-9e89-fb20f95ffc5d,'XML Developers Guide','Computer','2000-10-01','An in-depth look at creating applications with XML.',{'Gambardella, Matthew','Marcin Miklas','Fryderyk Zajac','Anna Zajac'},112291,{'D':3.85,'E':4.11,'F':4.00},'white and soft')")));

			assertTrue(rc.toString(), rc.success.contains(new CqlQuery(CqlQueryType.UNKNOWN,
					"INSERT INTO MyBooks (id,title,pages,price) VALUES (644556a9-651b-45b3-bd01-cc807c64bd4f,'some text in title ;)',112291,{'DE':4,'EU':34})")));

			assertTrue(rc.toString(), rc.success.contains(new CqlQuery(CqlQueryType.UNKNOWN,
					"INSERT INTO MyBooks (id,title,pages,price) VALUES (38710416-6253-4a77-a31c-6429f16f3837,'just title..... NR 2',112291,{'DE':4,'EU':4})")));

			assertTrue(rc.toString(), rc.success.contains(new CqlQuery(CqlQueryType.UNKNOWN,
					"INSERT INTO MyBooks (id,title,pages,price) VALUES (c746c90c-94dc-45dc-9b47-e410e46a0e61,'just title..... urrr...',112291,{'DE':44,'EU':343})")));

			assertTrue(rc.toString(), rc.success.contains(new CqlQuery(CqlQueryType.UNKNOWN,
					"INSERT INTO MyBooks (id,title,pages,price) VALUES (6ceb1c47-0955-4654-80d4-5230b88467d2,'just title.....',112291,{'DE':4,'EU':34})")));

			assertTrue(rc.toString(), rc.success.contains(new CqlQuery(CqlQueryType.UNKNOWN,
					"INSERT INTO MyBooks (id,                title,pages,price) VALUES (4d8c695c-a8e9-4f74-8462-1def6ec075d3,'just dasd title.....',112291,{'DE':4,'EU':344})")));

			assertTrue(rc.toString(), rc.success.contains(new CqlQuery(CqlQueryType.UNKNOWN,
					"INSERT INTO MyBooks (id,title,pages,            price) VALUES (e1390b2e-1393-490b-aa6e-88874ac1fc88,            'just title. wed dwe....',112291,           {'DE':4,'EU':324})")));
		}

		assertFalse(queryService.execute(testCql).isEmpty());
		assertEquals("some text in title ;)", queryService.execute(testCql).rows.iterator().next().getString("title"));

	}

	public class ResultConsumer implements ResultWritter {

		public List<CqlQuery> success = new ArrayList<>();

		public Map<CqlQuery, QueryException> error = new HashMap<>();

		@Override
		public void success(CqlQuery query, long runtime) {
			assertTrue(runtime >= 0);
			success.add(query);
		}

		@Override
		public void error(CqlQuery query, QueryException errore, long runtime) {
			assertTrue(runtime >= 0);
			error.put(query, errore);
		}

		public int size() {
			return success.size() + error.size();
		}

		@Override
		public String toString() {
			return "ResultConsumer [success=" + success + ", error=" + error + "]";
		}

	}
}

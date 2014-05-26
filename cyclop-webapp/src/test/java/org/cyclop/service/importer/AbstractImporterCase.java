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
package org.cyclop.service.importer;

import com.datastax.driver.core.Row;
import org.cyclop.model.CqlQuery;
import org.cyclop.model.CqlQueryResult;
import org.cyclop.model.CqlQueryType;
import org.cyclop.service.cassandra.QueryService;
import org.cyclop.service.importer.model.ImportConfig;
import org.cyclop.service.importer.model.ImportStats;
import org.cyclop.test.AbstractTestCase;
import org.junit.Before;
import org.junit.Test;

import javax.inject.Inject;
import java.io.InputStream;
import java.util.Set;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

/** @author Maciej Miklas */
public abstract class AbstractImporterCase extends AbstractTestCase {

	@Inject
	private QueryService queryService;

	abstract QueryImporter getImporter();

	@Test
	public void testImportOneQueryPerLine() throws Exception {
		try (InputStream fio = getClass().getResourceAsStream("/cql/createDemoData.cql")) {
			ResultConsumer rc = new ResultConsumer();
			ImportStats stats = getImporter()
					.importScript(fio, rc, new ImportConfig().withContinueWithErrors(true).withUpdateHistory(true));
			assertEquals(rc.toString(), 6, rc.size());
			assertEquals(rc.error.toString(), 0, rc.error.size());
			assertEquals(rc.toString(), 6, rc.success.size());
			assertEquals(rc.toString(), 0, stats.errorCount);
			assertEquals(rc.toString(), 6, stats.successCount);
			assertTrue(rc.toString(), rc.success.contains(new CqlQuery(CqlQueryType.UNKNOWN,
					"INSERT INTO MyBooks (id,title,pages,price) VALUES (0f6939a7-62f7-4ed0-a909-6fc302764c8d,'just title.....',112291,{'DE':4,'EU':34})")));
		}
	}

	@Test
	public void testImportLineBreaks() throws Exception {
		queryService.execute(new CqlQuery(CqlQueryType.SELECT,
				"delete from CqlDemo.MyBooks where id=644556a9-651b-45b3-bd01-cc807c64bd4f"));

		CqlQuery testCql = new CqlQuery(CqlQueryType.SELECT,
				"select title from cqldemo.mybooks where id=644556a9-651b-45b3-bd01-cc807c64bd4f");
		assertTrue(queryService.execute(testCql).isEmpty());

		try (InputStream fio = getClass().getResourceAsStream("/cql/testImportLineBreaks.cql")) {
			ResultConsumer rc = new ResultConsumer();
			ImportStats stats = getImporter()
					.importScript(fio, rc, new ImportConfig().withContinueWithErrors(true).withUpdateHistory(true));
			assertEquals(8, rc.size());
			assertEquals(rc.error.toString(), 0, rc.error.size());
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
		assertEquals("some text in title ;)", queryService.execute(testCql).iterator().next().getString("title"));

	}

	@Test
	public void testImportAmount_1() throws Exception {
		execImport(1, 1, 0, 1, 1, 1);
	}

	@Test
	public void testImportAmount_3() throws Exception {
		execImport(3, 3, 0, 1, 3, 3);
	}

	@Test
	public void testImportAmount_6() throws Exception {
		execImport(6, 6, 0, 4, 9, 6);
	}

	@Test
	public void testImportAmount_7() throws Exception {
		execImport(7, 7, 0, 4, 10, 7);
	}

	@Test
	public void testImportAmount_8() throws Exception {
		execImport(8, 8, 0, 4, 11, 8);
	}

	@Test
	public void testImportAmount_9() throws Exception {
		execImport(9, 9, 0, 4, 12, 9);
	}

	@Test
	public void testImportAmount_15() throws Exception {
		execImport(15, 15, 0, 0, 14, 15);
	}

	@Test
	public void testImportAmount_32() throws Exception {
		execImport(31, 32, 0, 0, 30, 32);
	}

	@Test
	public void testImportAmount_116() throws Exception {
		execImport(119, 116, 0, 0, 119, 116);
	}

	private void execImport(int impcolSize, int scriptQueries, int amountError, int vmin, int vmax, int scrNr)
			throws Exception {
		try (InputStream fio = getClass().getResourceAsStream("/cql/testImportOneCol_" + scrNr + ".cql")) {
			ResultConsumer rc = new ResultConsumer();
			ImportStats stats = getImporter()
					.importScript(fio, rc, new ImportConfig().withContinueWithErrors(false).withUpdateHistory(true));

			assertEquals(scriptQueries + amountError, rc.size());
			assertEquals(amountError, rc.error.size());
			assertEquals(scriptQueries, rc.success.size());
			assertEquals(amountError, stats.errorCount);
			assertEquals(scriptQueries, stats.successCount);
		}

		{
			CqlQueryResult res = queryService.execute(new CqlQuery(CqlQueryType.SELECT,
					"select impcol from CqlDemo.MyBooks where id=44a2054c-f98b-43a7-833d-0e1358fdaa82"));

			assertEquals(1, res.iterator().rowMetadata.commonColumns.size());
			assertEquals(0, res.iterator().rowMetadata.dynamicColumns.size());

			for (Row row : res) {
				Set<Integer> impcol = row.getSet("impcol", Integer.class);
				assertEquals(impcolSize, impcol.size());
				for (Integer val : impcol) {
					assertTrue(val + ">=" + vmin, val >= vmin);
					assertTrue(val + "<=" + vmax, val <= vmax);
				}
			}
		}
	}

	@Before
	public void before() throws Exception {
		super.setup();
		queryService.execute(new CqlQuery(CqlQueryType.SELECT,
				"delete from CqlDemo.MyBooks where id=44a2054c-f98b-43a7-833d-0e1358fdaa82"));

		CqlQueryResult res = queryService.execute(new CqlQuery(CqlQueryType.SELECT,
				"select impcol from CqlDemo.MyBooks where id=44a2054c-f98b-43a7-833d-0e1358fdaa82"));
		assertTrue(res.toString(), res.isEmpty());
	}
}

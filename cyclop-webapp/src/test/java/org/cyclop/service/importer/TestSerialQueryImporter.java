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

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.io.InputStream;

import javax.inject.Inject;
import javax.inject.Named;

import org.cyclop.model.CqlQuery;
import org.cyclop.model.CqlQueryType;
import org.cyclop.service.importer.model.ImportConfig;
import org.cyclop.service.importer.model.ImportStats;
import org.junit.Ignore;
import org.junit.Test;

public class TestSerialQueryImporter extends AbstractImporterCase {

	@Inject
	@Named(QueryImporter.IMPL_SERIAL)
	private QueryImporter importer;

	@Test
	public void testBreakAfterError() throws Exception {

		try (InputStream fio = getClass().getResourceAsStream("/cql/testImportOrdered.cql")) {
			ResultConsumer rc = new ResultConsumer();
			ImportStats stats = importer.importScript(fio, rc, new ImportConfig().withContinueWithErrors(false)
					.withUpdateHistory(true));

			assertEquals(rc.toString(), 3, rc.size());
			assertEquals(rc.toString(), 1, rc.error.size());
			assertEquals(rc.toString(), 2, rc.success.size());
			assertEquals(rc.toString(), 1, stats.errorCount);
			assertEquals(rc.toString(), 2, stats.successCount);
		}
	}
	@Ignore
	@Test
	public void testImportOrdered() throws Exception {
		try (InputStream fio = getClass().getResourceAsStream("/cql/testImportOrdered.cql")) {
			ResultConsumer rc = new ResultConsumer();
			ImportStats stats = importer.importScript(fio, rc, new ImportConfig().withContinueWithErrors(true)
					.withUpdateHistory(true));

			assertTrue(rc.toString(), rc.success.contains(new CqlQuery(CqlQueryType.UNKNOWN, "USE CqlDemo")));
			assertTrue(
					rc.toString(),
					rc.success
							.contains(new CqlQuery(
									CqlQueryType.UNKNOWN,
									"INSERT INTO MyBooks (id,title,genre,publishDate,description,authors,pages,price,paperType) VALUES (1ff18f41-cfb8-45ff-9e89-fb20f95ffc5d,'XML Developers Guide','Computer','2000-10-01','An in-depth look at creating applications with XML.',{'Gambardella, Matthew','Marcin Miklas','Fryderyk Zajac','Anna Zajac'},112291,{'D':3.85,'E':4.11,'F':4.00},'white and soft')")));
			assertTrue(
					rc.toString(),
					rc.success
							.contains(new CqlQuery(
									CqlQueryType.UNKNOWN,
									"INSERT INTO MyBooks (id,title,pages,price) VALUES (44f2054c-f98b-43a7-833d-0e1358fdee82,'just title.....',112291,{'DE':4,'EU':34})")));
			assertTrue(
					rc.toString(),
					rc.success
							.contains(new CqlQuery(
									CqlQueryType.UNKNOWN,
									"INSERT INTO MyBooks (id,title,pages,price) VALUES (c746c90c-94dc-45dc-9b47-e410e46a0e61,'just title..... urrr...',112291,{'DE':44,'EU':343})")));
			assertTrue(
					rc.toString(),
					rc.success
							.contains(new CqlQuery(
									CqlQueryType.UNKNOWN,
									"INSERT INTO MyBooks (id,title,pages,            price) VALUES (e1390b2e-1393-490b-aa6e-88874ac1fc88,            'just title. wed dwe....',112291,           {'DE':4,'EU':324})")));

			for (int i = 1; i < 12; i++) {

				String q1 = "ALTER TABLE MyBooks ADD tc_" + i + " varchar";
				assertTrue(q1, rc.success.contains(new CqlQuery(CqlQueryType.UNKNOWN, q1)));

				String q2 = "INSERT INTO MyBooks (id,tc_" + i + ") VALUES (44f2054c-f98b-43a7-833d-0e1358fdee82,'v" + i
						+ "')";
				assertTrue(rc.toString(), rc.success.contains(new CqlQuery(CqlQueryType.UNKNOWN, q2)));
			}

			{
				Exception res = rc.error.get(new CqlQuery(CqlQueryType.UNKNOWN,
						"asdf adf adfa;sdf as'fasdf;asdf ;;q34t24gtvrf"));
				assertNotNull(rc.toString(), res);
				assertTrue(res.toString(), res.getMessage().contains("Error executing CQL"));
				assertTrue(res.toString(),
						res.getMessage().contains("reason: line 1:45 mismatched character '<EOF>' expecting"));
			}

			{
				Exception res = rc.error
						.get(new CqlQuery(
								CqlQueryType.UNKNOWN,
								"INSERT INTO MyBooks (id,title,pages,price) VALUE (38710416-6253-4a77-a31c-6429f16f3837,'just title..... NR 2',112291,{'DE':4,'EU':4})"));
				assertNotNull(rc.toString(), res);
				assertTrue(res.toString(), res.getMessage().contains("Error executing CQL"));
				assertTrue(res.toString(),
						res.getMessage().contains("reason: line 1:43 mismatched input 'VALUE' expecting K_VALUES"));
			}

			{
				Exception res = rc.error
						.get(new CqlQuery(CqlQueryType.UNKNOWN,
								"INSERT INTO MyBooks (id,title,pages,price) VALUES (6ceb1c47-0955-4654-80d4-5230b88467d2,'just title.....',112291)"));
				assertNotNull(rc.toString(), res);
				assertTrue(res.toString(), res.getMessage().contains("Error executing CQL"));
				assertTrue(res.toString(), res.getMessage().contains("Unmatched column names/values"));
			}

			{
				Exception res = rc.error.get(new CqlQuery(CqlQueryType.UNKNOWN,
						"INSERT INTO MyBooks (id,tc_13XYZ) VALUES (44f2054c-f98b-43a7-833d-0e1358fdee82,'v13')"));
				assertNotNull(rc.toString(), res);
				assertTrue(res.toString(), res.getMessage().contains("Error executing CQL"));
				assertTrue(res.toString(), res.getMessage().contains("Unknown identifier tc_13"));
			}

			{
				assertEquals(4, rc.error.size());
				assertEquals(4, stats.errorCount);
				assertEquals(2044, rc.success.size());
				assertEquals(rc.success.size(), stats.successCount);
				assertEquals(2048, rc.size());
			}

		}
	}

	@Override
	QueryImporter getImporter() {
		return importer;
	}
}

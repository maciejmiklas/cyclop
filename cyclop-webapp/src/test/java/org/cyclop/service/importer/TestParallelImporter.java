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

import static org.junit.Assert.assertTrue;

import java.io.InputStream;

import javax.inject.Inject;
import javax.inject.Named;

import org.cyclop.service.importer.model.ImportConfig;
import org.cyclop.service.importer.model.ImportStats;
import org.junit.Test;

/** @author Maciej Miklas */
public class TestParallelImporter extends AbstractImporterCase {

	@Inject
	@Named(QueryImporter.IMPL_PARALLEL)
	private QueryImporter importer;

	@Override
	QueryImporter getImporter() {
		return importer;
	}

	@Override
	public void testImportOneQueryPerLine() throws Exception {
	}

	@Override
	public void testImportLineBreaks() throws Exception {
	}

	@Test
	public void testBreakAfterError() throws Exception {

		try (InputStream fio = getClass().getResourceAsStream("/cql/testImportOrdered.cql")) {
			ResultConsumer rc = new ResultConsumer();
			ImportStats stats = importer.importScript(fio, rc, new ImportConfig().withContinueWithErrors(false)
					.withUpdateHistory(true));

			assertTrue(rc.toString(), rc.size() < 100);
			assertTrue(rc.toString(), rc.error.size() < 4);
			assertTrue(rc.toString(), rc.success.size() < 100);
			assertTrue(rc.toString(), stats.errorCount < 4);
			assertTrue(rc.toString(), stats.successCount < 100);
		}
	}
}

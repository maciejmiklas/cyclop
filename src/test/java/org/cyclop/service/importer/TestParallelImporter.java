package org.cyclop.service.importer;

import org.cyclop.service.importer.model.ImportConfig;
import org.cyclop.service.importer.model.ImportStats;
import org.junit.Test;

import javax.inject.Inject;
import javax.inject.Named;
import java.io.InputStream;

import static org.junit.Assert.assertTrue;

/** @author Maciej Miklas */
public class TestParallelImporter extends AbstractImporterCase {

	@Inject
	@Named(QueryImporter.IMPL_PARALLEL)
	private QueryImporter importer;

	@Override
	QueryImporter getImporter() {
		return importer;
	}

	@Test
	public void testBreakAfterError() throws Exception {

		try (InputStream fio = getClass().getResourceAsStream("/cql/testImportOrdered.cql")) {
			ResultConsumer rc = new ResultConsumer();
			ImportStats stats = importer
					.importScript(fio, rc, new ImportConfig().withContinueWithErrors(false).withUpdateHistory(true));

			assertTrue(rc.toString(), rc.size() < 100);
			assertTrue(rc.toString(), rc.error.size() < 4);
			assertTrue(rc.toString(), rc.success.size() < 100);
			assertTrue(rc.toString(), stats.errorCount < 4);
			assertTrue(rc.toString(), stats.successCount < 100);
		}
	}
}

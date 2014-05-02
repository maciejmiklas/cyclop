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
import javax.inject.Named;
import java.io.InputStream;
import java.util.Set;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

/** @author Maciej Miklas */
public class TestParallelImporter extends AbstractTestCase {

	@Inject
	@Named(QueryImporter.IMPL_PARALLEL)
	private QueryImporter importer;

	@Inject
	private QueryService queryService;

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
		try (InputStream fio = getClass().getResourceAsStream("/cql/testImportParallel_" + scrNr + ".cql")) {
			ResultConsumer rc = new ResultConsumer();
			ImportStats stats = importer
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

			assertEquals(1, res.commonColumns.size());
			assertEquals(0, res.dynamicColumns.size());
			assertEquals(1, res.rows.size());

			for (Row row : res.rows) {
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
	public void before() {
		queryService.execute(new CqlQuery(CqlQueryType.SELECT,
				"delete from CqlDemo.MyBooks where id=44a2054c-f98b-43a7-833d-0e1358fdaa82"));

		CqlQueryResult res = queryService.execute(new CqlQuery(CqlQueryType.SELECT,
				"select impcol from CqlDemo.MyBooks where id=44a2054c-f98b-43a7-833d-0e1358fdaa82"));
		assertTrue(res.toString(), res.isEmpty());
	}
}

package org.cyclop.service.exporter;

import org.cyclop.model.CqlQuery;
import org.cyclop.model.CqlQueryResult;
import org.cyclop.model.CqlQueryType;
import org.cyclop.service.cassandra.QueryService;
import org.cyclop.test.AbstractTestCase;
import org.junit.Test;

import javax.inject.Inject;
import java.util.Scanner;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

public class TestCsvQueryResultExporter extends AbstractTestCase {

	@Inject
	private CsvQueryResultExporter exporter;

	@Inject
	private QueryService qs;

	@Test
	public void testExportAsCsv_ResultEmpty() {
		CqlQuery query = new CqlQuery(CqlQueryType.SELECT, "select id from cqldemo.mybooks where pages=-1");
		CqlQueryResult queryRes = qs.execute(query);
		String exportRsult = exporter.exportAsCsv(query, queryRes);
		assertNotNull(exportRsult);

		Scanner scanner = new Scanner(exportRsult);
		int lineIndex = 0;
		while (scanner.hasNextLine()) {
			String line = scanner.nextLine();

			switch (lineIndex) {
				case 0:
					assertEquals("select id from cqldemo.mybooks where pages=-1", line);
					break;
				case 1:
					assertEquals("====", line);
					break;
			}
			lineIndex++;
		}
	}

	@Test
	public void testExportAsCsv_SelectIdsFromMyBooks() {
		CqlQuery query = new CqlQuery(CqlQueryType.SELECT, "select id from cqldemo.mybooks where pages=112291");
		CqlQueryResult queryRes = qs.execute(query);
		String exportRsult = exporter.exportAsCsv(query, queryRes);
		assertNotNull(exportRsult);

		Scanner scanner = new Scanner(exportRsult);
		int lineIndex = 0;
		while (scanner.hasNextLine()) {

			String line = scanner.nextLine();

			switch (lineIndex) {
				case 0:
					assertEquals("select id from cqldemo.mybooks where pages=112291", line);
					break;
				case 1:
					assertEquals("====", line);
					break;
				case 2:
					assertEquals("\"id\"", line);
					break;
				case 3:
					assertEquals("\"43da06c5-bf5c-4468-b92c-cb8aacb29675\"", line);
					break;
				case 4:
					assertEquals("\"0f6939a7-62f7-4ed0-a909-6fc302764c8d\"", line);
					break;
				case 5:
					assertEquals("\"46c63542-db1a-452e-97c9-e3f14b7ebdae\"", line);
					break;
				case 6:
					assertEquals("\"c9c581fd-7c25-4b65-b0ce-a65bd4ec4176\"", line);
					break;
				case 7:
					assertEquals("\"1ff18f41-cfb8-45ff-9e89-fb20f95ffc5d\"", line);
					break;
			}
			lineIndex++;
		}
	}

	@Test
	public void testExportAsCsv_SelectAutorsAndGerneFromMyBooks() {
		CqlQuery query = new CqlQuery(CqlQueryType.SELECT,
				"select authors, genre from cqldemo.mybooks where pages=112291");
		CqlQueryResult queryRes = qs.execute(query);
		String exportRsult = exporter.exportAsCsv(query, queryRes);
		assertNotNull(exportRsult);

		Scanner scanner = new Scanner(exportRsult);
		int lineIndex = 0;
		while (scanner.hasNextLine()) {

			String line = scanner.nextLine();

			switch (lineIndex) {
				case 0:
					assertEquals("select authors, genre from cqldemo.mybooks where pages=112291", line);
					break;
				case 1:
					assertEquals("====", line);
					break;
				case 2:
					assertEquals("\"authors\";\"genre\"", line);
					break;
				case 3:
					assertEquals(
							"\"\"Anna Zajac\",\"Fryderyk Zajac\",\"Gambardella, Matthew\",\"Marcin Miklas 2\"\";\"Computer\"",
							line);
					break;
				case 4:
					assertEquals("\"\";\"\"", line);
					break;
				case 5:
					assertEquals(
							"\"\"A-066a7f59-faef-4cf0-89b6-5c6b3f8c8dcf\",\"b\",\"c\",\"d\",\"e-979cb909-c7dc-4880-ad6a-a8f1388c5939\",\"f\",\"g\",\"h\",\"i\",\"j\",\"k\",\"l\",\"m\",\"n\"\";\"\"",
							line);
					break;
				case 6:
					assertEquals("\"\"Gambardella, Matthew\"\";\"Computer\"", line);
					break;
				case 7:
					assertEquals(
							"\"\"Anna Zajac\",\"Fryderyk Zajac\",\"Gambardella, Matthew\",\"Marcin Miklas\"\";\"Computer\"",
							line);
					break;
			}
			lineIndex++;
		}
	}

	@Test
	public void testExportAsCsv_SelectAllFromMyBooks() {
		CqlQuery query = new CqlQuery(CqlQueryType.SELECT, "select * from cqldemo.mybooks where pages=112291");
		CqlQueryResult queryRes = qs.execute(query);
		String exportRsult = exporter.exportAsCsv(query, queryRes);
		assertNotNull(exportRsult);

		Scanner scanner = new Scanner(exportRsult);
		int lineIndex = 0;
		while (scanner.hasNextLine()) {

			String line = scanner.nextLine();

			switch (lineIndex) {
				case 0:
					assertEquals("select * from cqldemo.mybooks where pages=112291", line);
					break;
				case 1:
					assertEquals("====", line);
					break;
				case 2:
					assertEquals(
							"\"id\";\"authors\";\"description\";\"genre\";\"pages\";\"papertype\";\"price\";\"publishdate\";\"title\"",
							line);
					break;
				case 3:
					System.out.println(
							"\"43da06c5-bf5c-4468-b92c-cb8aacb29675\";\"\"Anna Zajac\",\"Fryderyk Zajac\",\"Gambardella, Matthew\",\"Marcin Miklas 2\"\";\"An in-depth look at creating 2 applications with XML.\";\"Computer\";\"112291\";\"white and soft3\";\"\"D\"=\"3.45\",\"E\"=\"2.11\",\"F\"=\"4.3\"\";\"2000-10-02 00:00:00.000\";\"XML Developers Guide 2\"");
					System.out.println(line);
					assertEquals(
							"\"43da06c5-bf5c-4468-b92c-cb8aacb29675\";\"\"Anna Zajac\",\"Fryderyk Zajac\",\"Gambardella, Matthew\",\"Marcin Miklas 2\"\";\"An in-depth look at creating 2 applications with XML.\";\"Computer\";\"112291\";\"white and soft3\";\"\"D\"=\"3.45\",\"E\"=\"2.11\",\"F\"=\"4.3\"\";\"2000-10-02 00:00:00.000\";\"XML Developers Guide 2\"",
							line);
					break;
				case 4:
					assertEquals(
							"\"0f6939a7-62f7-4ed0-a909-6fc302764c8d\";\"\";\"\";\"\";\"112291\";\"\";\"\"DE\"=\"4.0\",\"EU\"=\"34.0\"\";\"\";\"just title.....\"",
							line);
					break;
				case 5:
					assertEquals(
							"\"46c63542-db1a-452e-97c9-e3f14b7ebdae\";\"\"A-066a7f59-faef-4cf0-89b6-5c6b3f8c8dcf\",\"b\",\"c\",\"d\",\"e-979cb909-c7dc-4880-ad6a-a8f1388c5939\",\"f\",\"g\",\"h\",\"i\",\"j\",\"k\",\"l\",\"m\",\"n\"\";\"\";\"\";\"112291\";\"\";\"\";\"\";\"\"",
							line);
					break;
				case 6:
					assertEquals(
							"\"c9c581fd-7c25-4b65-b0ce-a65bd4ec4176\";\"\"Gambardella, Matthew\"\";\"An in-depth look at creating applications with XML.\";\"Computer\";\"112291\";\"white und soft\";\"\"D\"=\"3.85\"\";\"2012-10-01 00:00:00.000\";\"XML Developers Guide 3\"",
							line);
					break;
				case 7:
					assertEquals(
							"\"1ff18f41-cfb8-45ff-9e89-fb20f95ffc5d\";\"\"Anna Zajac\",\"Fryderyk Zajac\",\"Gambardella, Matthew\",\"Marcin Miklas\"\";\"An in-depth look at creating applications with XML.\";\"Computer\";\"112291\";\"white and soft\";\"\"D\"=\"3.85\",\"E\"=\"4.11\",\"F\"=\"4.0\"\";\"2000-10-01 00:00:00.000\";\"XML Developers Guide\"",
							line);
					break;
			}
			lineIndex++;
		}
	}
}

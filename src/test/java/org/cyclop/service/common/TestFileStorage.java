package org.cyclop.service.common;

import org.cyclop.common.AppConfig;
import org.cyclop.model.CqlQuery;
import org.cyclop.model.CqlQueryName;
import org.cyclop.model.QueryEntry;
import org.cyclop.model.QueryFavourites;
import org.cyclop.model.QueryHistory;
import org.cyclop.model.UserIdentifier;
import org.cyclop.test.AbstractTestCase;
import org.junit.Before;
import org.junit.Test;

import javax.inject.Inject;
import java.io.File;
import java.util.Iterator;
import java.util.List;
import java.util.NoSuchElementException;
import java.util.Set;
import java.util.UUID;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

/** @author Maciej Miklas */
public class TestFileStorage extends AbstractTestCase {

	@Inject
	private AppConfig config;

	@Inject
	private FileStorage storage;

	@Before
	public void init() throws Exception {
		File histFolder = new File(config.fileStore.folder);
		histFolder.setWritable(true);
		assertTrue("History folder not writable:" + histFolder, histFolder.canWrite());
	}

	@Test
	public void testSupported_OnWritableFolder() {
		assertTrue(storage.supported());
		assertTrue(storage.checkSupported());
	}

	@Test
	public void testSupported_OnReadOnlyFolder() {

		// TODO could be provided as annotation @LinuxOnlyTest
		if (!unixOs) {
			return;
		}
		File histFolder = new File(config.fileStore.folder);
		histFolder.setWritable(false);

		assertTrue(storage.supported());
		assertFalse(storage.checkSupported());
	}

	@Test
	public void testRead_FileNotFound() {
		QueryHistory readHistory = storage.read(new UserIdentifier(UUID.randomUUID()), QueryHistory.class);
		assertNull(readHistory);
	}

	@Test
	public void testEmptyHistory() {
		QueryHistory hist = new QueryHistory();
		assertEquals(0, hist.size());
		assertFalse(hist.iterator().hasNext());

		CqlQuery query = new CqlQuery(CqlQueryName.SELECT, "select * from MyTable");
		QueryEntry histEntry = new QueryEntry(query);
		assertFalse(hist.contains(histEntry));
		try {
			hist.iterator().next();
			fail();
		} catch (NoSuchElementException e) {
			// OK
		}
	}

	@Test
	public void testEmptyFavourites() {
		QueryFavourites hist = new QueryFavourites();
		assertEquals(0, hist.size());
		assertFalse(hist.copyAsSortedSet().iterator().hasNext());

		CqlQuery query = new CqlQuery(CqlQueryName.SELECT, "select * from MyTable");
		QueryEntry histEntry = new QueryEntry(query);
		assertFalse(hist.contains(histEntry));
		try {
			hist.copyAsSortedSet().iterator().next();
			fail();
		} catch (NoSuchElementException e) {
			// OK
		}
	}

	@Test
	public void testCreateAndRead_SingleHistoryEntry() {
		UserIdentifier userId = new UserIdentifier(UUID.randomUUID());
		QueryHistory newHistory = new QueryHistory();

		CqlQuery query = new CqlQuery(CqlQueryName.SELECT, "select * from MyTable");
		QueryEntry histEntry = new QueryEntry(query);
		newHistory.add(histEntry);

		storage.store(userId, newHistory);

		QueryHistory readHistory = storage.read(userId, QueryHistory.class);
		assertNotNull(readHistory);
		assertEquals(1, readHistory.size());
		int idx = 0;
		List<QueryEntry> histList = readHistory.copyAsList();
		try (QueryHistory.HistoryIterator historyIterator = readHistory.iterator()) {
			assertTrue(historyIterator.hasNext());
			QueryEntry readEntry = historyIterator.next();
			assertEquals(histList.get(idx++), readEntry);
			assertEquals(query, readEntry.query);
			assertEquals(histEntry.executedOnUtc.toString(), readEntry.executedOnUtc.toString());

			assertEquals(0, storage.getLockRetryCount());
		}
	}

	@Test
	public void testCreateAndRead_SingleFavouritesEntry() {
		UserIdentifier userId = new UserIdentifier(UUID.randomUUID());
		QueryFavourites newHistory = new QueryFavourites();

		CqlQuery query = new CqlQuery(CqlQueryName.SELECT, "select * from MyTable");
		QueryEntry favEntry = new QueryEntry(query);
		newHistory.addWithSizeCheck(favEntry);

		storage.store(userId, newHistory);

		QueryFavourites readHistory = storage.read(userId, QueryFavourites.class);
		assertNotNull(readHistory);
		assertEquals(1, readHistory.size());
		assertTrue(readHistory.copyAsSortedSet().iterator().hasNext());
		QueryEntry readEntry = readHistory.copyAsSortedSet().iterator().next();
		assertEquals(query, readEntry.query);
		assertEquals(favEntry.executedOnUtc.toString(), readEntry.executedOnUtc.toString());
		assertEquals(0, storage.getLockRetryCount());
	}

	@Test
	public void testLimitFavourites() {
		UserIdentifier userId = new UserIdentifier(UUID.randomUUID());
		QueryFavourites history = new QueryFavourites();
		{
			for (int i = 0; i < 50; i++) {
				CqlQuery query = new CqlQuery(CqlQueryName.SELECT, "select * from MyTable where id=" + i);
				assertTrue(history.addWithSizeCheck(new QueryEntry(query)));
			}
			assertEquals(50, history.size());
		}

		checkFavLimitContent(history);

		{
			for (int i = 0; i < 10; i++) {
				CqlQuery query = new CqlQuery(CqlQueryName.SELECT, "select * from MyTableA where id=" + i);
				assertFalse(history.addWithSizeCheck(new QueryEntry(query)));
			}
			checkFavLimitContent(history);
		}

		{
			for (int i = 0; i < 20; i++) {
				CqlQuery query = new CqlQuery(CqlQueryName.SELECT, "select * from MyTable where id=" + i);
				assertTrue(history.addWithSizeCheck(new QueryEntry(query)));
			}
			checkFavLimitContent(history);
		}

		{
			storage.store(userId, history);
			QueryFavourites readHistory = storage.read(userId, QueryFavourites.class);
			checkFavLimitContent(readHistory);
			checkFavLimitContent(history);
		}
	}

	private void checkFavLimitContent(QueryFavourites history) {
		Set<QueryEntry> fav = history.copyAsSortedSet();
		Iterator<QueryEntry> favIt = fav.iterator();
		assertEquals(50, fav.size());
		assertEquals(50, history.size());
		for (int i = 0; i < 50; i++) {
			assertTrue(favIt.hasNext());
			CqlQuery query = new CqlQuery(CqlQueryName.SELECT, "select * from MyTable where id=" + i);
			assertTrue(history.contains(new QueryEntry(query)));
			assertTrue(history.contains(favIt.next()));
		}
	}

	@Test
	public void testEvictHistory() {
		UserIdentifier userId = new UserIdentifier(UUID.randomUUID());

		{
			QueryHistory history = new QueryHistory();

			for (int i = 0; i < 600; i++) {
				CqlQuery query = new CqlQuery(CqlQueryName.SELECT, "select * from MyTable1 where id=" + i);
				history.add(new QueryEntry(query));
			}
			assertEquals(500, history.size());

			try (QueryHistory.HistoryIterator hit = history.iterator()) {
				for (int i = 499; i >= 0; i--) {
					assertTrue(hit.hasNext());
					QueryEntry entry = hit.next();
					assertTrue(history.contains(entry));
					assertNotNull(entry.executedOnUtc);
					assertEquals("select * from MyTable1 where id=" + (i + 100), entry.query.part);
				}
			}

			storage.store(userId, history);
		}

		{
			QueryHistory history = storage.read(userId, QueryHistory.class);
			assertNotNull(history);
			assertEquals(500, history.size());
			try (QueryHistory.HistoryIterator hit = history.iterator()) {
				for (int i = 499; i > 0; i--) {
					assertTrue(hit.hasNext());
					QueryEntry entry = hit.next();
					assertTrue(history.contains(entry));
					assertNotNull(entry.executedOnUtc);
					assertEquals("select * from MyTable1 where id=" + (i + 100), entry.query.part);
				}
			}

			for (int i = 0; i < 10; i++) {
				CqlQuery query = new CqlQuery(CqlQueryName.SELECT, "select * from MyTable2 where id=" + i);
				history.add(new QueryEntry(query));
			}
			storage.store(userId, history);
		}

		{
			QueryHistory history = storage.read(userId, QueryHistory.class);
			assertNotNull(history);
			assertEquals(500, history.size());

			try (QueryHistory.HistoryIterator hit = history.iterator()) {
				for (int i = 499; i > 489; i--) {
					assertTrue(hit.hasNext());
					QueryEntry entry = hit.next();
					assertTrue(history.contains(entry));
					assertNotNull(entry.executedOnUtc);
					assertEquals("select * from MyTable2 where id=" + (i - 490), entry.query.part);
				}
				for (int i = 489; i > 0; i--) {
					assertTrue(hit.hasNext());
					QueryEntry entry = hit.next();
					assertTrue(history.contains(entry));
					assertNotNull(entry.executedOnUtc);
					assertEquals("select * from MyTable1 where id=" + (i + 110), entry.query.part);
				}
			}
		}
	}

}

package org.cyclop.service.history.impl;

import com.google.common.collect.UnmodifiableIterator;
import org.cyclop.model.CqlQuery;
import org.cyclop.model.CqlQueryName;
import org.cyclop.model.QueryHistory;
import org.cyclop.model.QueryHistoryEntry;
import org.cyclop.model.UserIdentifier;
import org.cyclop.service.common.FileStorage;
import org.cyclop.test.AbstractTestCase;
import org.cyclop.test.ThreadTestScope;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import javax.inject.Inject;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;

import static junit.framework.Assert.assertFalse;
import static junit.framework.Assert.assertNotSame;
import static junit.framework.Assert.assertNull;
import static junit.framework.Assert.assertSame;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

/** @author Maciej Miklas */
public class TestHistoryService extends AbstractTestCase {

	public static String CR = System.getProperty("line.separator");

	@Inject
	private HistoryServiceImpl historyService;

	@Inject
	private AsyncFileStore asyncFileStore;

	private UserIdentifier user;

	@Inject
	private FileStorage storage;

	@Inject
	private ThreadTestScope threadTestScope;

	@After
	public void cleanUp() {
		threadTestScope.setSingleThread(false);
	}

	@Before
	public void setup() {
		QueryHistory history = historyService.readHistory();
		assertNotNull(history);
		history.clearHistory();
		history.clearFavourite();

		assertEquals(0, history.historySize());
		assertEquals(0, history.favouritesSize());

		user = historyService.getUser();
		assertNotNull(user);
		assertNotNull(user.id);
	}

	@Test
	public void testCreateReadAndClear() throws Exception {
		QueryHistory history = historyService.readHistory();

		for (int i = 0; i < 20; i++) {
			history.addToHistory(new QueryHistoryEntry(new CqlQuery(CqlQueryName.SELECT, "select * " + CR + "from HistoryTest " +
					"where " + CR + "id=" + i)));

			assertTrue(history.addToFavouritesWithSizeCheck(new QueryHistoryEntry(new CqlQuery(CqlQueryName.SELECT,
					"select * from HistoryStarTest where id=" + i))));

			assertTrue(history.addToFavouritesWithSizeCheck(new QueryHistoryEntry(new CqlQuery(CqlQueryName.SELECT,
					"select * from HistoryStarTest where id=" + i))));

			historyService.store(history);
			QueryHistory historyQueue = asyncFileStore.getFromWriteQueue(user);
			assertNotNull(historyQueue);

			// should be the same instance
			assertSame(history, historyQueue);
		}
		assertEquals(20, history.favouritesSize());
		assertEquals(20, history.historySize());

		assertNull(storage.readHistory(user));

		asyncFileStore.flush();
		assertNull(asyncFileStore.getFromWriteQueue(user));

		assertSame(history, historyService.readHistory());

		QueryHistory readHist = storage.readHistory(user);
		assertNotSame(history, readHist);

		{
			int iterCount = 20;
			try (QueryHistory.HistoryIterator histIt = history.historyIterator()) {
				while (histIt.hasNext()) {
					iterCount--;
					QueryHistoryEntry entry = histIt.next();
					assertEquals("select * from HistoryTest where id=" + iterCount, entry.query.part);
					assertTrue(readHist + " - " + entry, readHist.containsHistory(entry));
					assertFalse(readHist + " - " + entry, readHist.containsFavourite(entry));
				}
			}
			assertEquals(0, iterCount);
		}

		{
			for (int i = 0; i < 20; i++) {
				QueryHistoryEntry entry = new QueryHistoryEntry(new CqlQuery(CqlQueryName.SELECT,
						"select * from HistoryStarTest where id=" + i));
				assertTrue(entry.toString(), history.containsFavourite(entry));
			}
		}

		{
			history.clearFavourite();
			assertEquals(0, history.favouritesSize());
			historyService.store(history);
			asyncFileStore.flush();
			assertEquals(0, storage.readHistory(user).favouritesSize());
		}

		{
			history.clearHistory();
			assertEquals(0, history.historySize());
			historyService.store(history);
			asyncFileStore.flush();
			assertEquals(0, storage.readHistory(user).historySize());
		}
	}

	@Test
	public void testMultiThreadForMultipleUsers() throws Exception {
		threadTestScope.setSingleThread(false);

		Set<QueryHistory> histories = executeMultiThreadTest(300);
		assertEquals(3, histories.size());
	}

	@Test
	public void testMultiThreadForSingleUsers() throws Exception {
		threadTestScope.setSingleThread(true);

		Set<QueryHistory> histories = executeMultiThreadTest(100);
		assertEquals(1, histories.size());
	}

	public Set<QueryHistory> executeMultiThreadTest(final int repeatInTest) throws Exception {
		ExecutorService executor = Executors.newFixedThreadPool(3);
		final Set<QueryHistory> histories = Collections.synchronizedSet(new HashSet<QueryHistory>());

		List<Callable<Void>> tasks = new ArrayList<>(3);
		final AtomicInteger executedCount = new AtomicInteger(0);
		for (int i = 0; i < 3; i++) {
			tasks.add(new Callable<Void>() {

				@Override
				public Void call() throws Exception {
					for (int i = 0; i < repeatInTest; i++) {
						QueryHistory history = historyService.readHistory();
						histories.add(history);

						QueryHistoryEntry histEntry = new QueryHistoryEntry(new CqlQuery(CqlQueryName.SELECT,
								"select * from MyTable2 where id=" + UUID.randomUUID()));
						history.addToHistory(histEntry);

						QueryHistoryEntry starredEntry = new QueryHistoryEntry(new CqlQuery(CqlQueryName.SELECT,
								"select * from MyTable2 where id=" + UUID.randomUUID()));
						int retry = 0;
						while (!history.addToFavouritesWithSizeCheck(starredEntry)) {
							retry++;
							assertTrue(retry < 100);

							UnmodifiableIterator<QueryHistoryEntry> iterator = history.copyOfFavourites().iterator();
							QueryHistoryEntry oldestToRemove = null;
							for (int a = 0; a < 20; a++) {
								oldestToRemove = iterator.next();
							}
							history.removeFavourite(oldestToRemove);
						}

						verifyHistEntry(history, histEntry, starredEntry);

						historyService.store(history);
						if (i % 20 == 0) {
							asyncFileStore.flush();
						}

						QueryHistory readHist = historyService.readHistory();
						verifyHistEntry(readHist, histEntry, starredEntry);

						executedCount.incrementAndGet();
						assertEquals(0, storage.getLockRetryCount());
					}
					return null;
				}

				void verifyHistEntry(QueryHistory history, QueryHistoryEntry histEntry, QueryHistoryEntry starredEntry) {
					assertNotNull(history);
					assertTrue("Starred (" + executedCount + "):" + starredEntry + " not found in: " + history,
							history.containsFavourite(starredEntry));

					assertTrue("History (" + executedCount + "):" + histEntry + " not found in: " + history,
							history.containsHistory(histEntry));
				}
			});
		}

		List<Future<Void>> results = executor.invokeAll(tasks);
		executor.shutdown();
		executor.awaitTermination(5, TimeUnit.MINUTES);

		for (Future<Void> result : results) {
			result.get();
		}
		assertEquals(3 * repeatInTest, executedCount.get());
		return histories;
	}

}

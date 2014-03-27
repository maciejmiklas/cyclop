package org.cyclop.service.queryprotocoling.impl;

import com.google.common.collect.ImmutableSortedSet;
import com.google.common.collect.UnmodifiableIterator;
import org.cyclop.model.CqlQuery;
import org.cyclop.model.CqlQueryType;
import org.cyclop.model.QueryEntry;
import org.cyclop.model.QueryFavourites;
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

import static junit.framework.Assert.assertNotSame;
import static junit.framework.Assert.assertNull;
import static junit.framework.Assert.assertSame;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

/** @author Maciej Miklas */
public class TestFavouritesService extends AbstractTestCase {

	public static String CR = System.getProperty("line.separator");

	@Inject
	private FavouritesServiceImpl favService;

	@Inject
	private AsyncFileStore<QueryFavourites> asyncFileStore;

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
		asyncFileStore.flush();

		QueryFavourites favs = favService.read();
		assertNotNull(favs);
		favs.clear();

		assertEquals(0, favs.size());

		user = favService.getUser();
		assertNotNull(user);
		assertNotNull(user.id);
	}

	@Test
	public void testCreateReadAndClear() throws Exception {
		QueryFavourites favourites = favService.read();

		for (int i = 0; i < 20; i++) {
			assertTrue(favourites.addWithSizeCheck(
					new QueryEntry(new CqlQuery(CqlQueryType.SELECT, "select * from HistoryStarTest where id=" + i),
							1000 + i, 34)));

			assertTrue(favourites.addWithSizeCheck(
					new QueryEntry(new CqlQuery(CqlQueryType.SELECT, "select * from HistoryStarTest where id=" + i),
							2000 + i, 34)));

			favService.store(favourites);
			QueryFavourites favQueue = asyncFileStore.getFromWriteQueue(user);
			assertNotNull(favQueue);

			// should be the same instance
			assertSame(favourites, favQueue);
		}
		assertEquals(20, favourites.size());

		assertNull(storage.read(user, QueryFavourites.class));

		asyncFileStore.flush();
		assertNull(asyncFileStore.getFromWriteQueue(user));

		assertSame(favourites, favService.read());

		QueryFavourites readFavs = storage.read(user, QueryFavourites.class);
		assertNotSame(favourites, readFavs);

		{
			for (int i = 0; i < 20; i++) {
				QueryEntry entry = new QueryEntry(
						new CqlQuery(CqlQueryType.SELECT, "select * from HistoryStarTest where id=" + i), 4000 + i, 34);
				assertTrue(entry.toString(), readFavs.contains(entry));
			}
		}

		{
			favourites.clear();
			assertEquals(0, favourites.size());
			favService.store(favourites);
			asyncFileStore.flush();
			assertEquals(0, storage.read(user, QueryFavourites.class).size());
		}

	}

	@Test
	public void testMultiThreadForMultipleUsers() throws Exception {
		threadTestScope.setSingleThread(false);

		Set<QueryFavourites> favs = executeMultiThreadTest(300);
		assertEquals(3, favs.size());
	}

	@Test
	public void testMultiThreadForSingleUsers() throws Exception {
		threadTestScope.setSingleThread(true);

		Set<QueryFavourites> favs = executeMultiThreadTest(100);
		assertEquals(1, favs.size());
	}

	public Set<QueryFavourites> executeMultiThreadTest(final int repeatInTest) throws Exception {
		ExecutorService executor = Executors.newFixedThreadPool(3);
		final Set<QueryFavourites> favs = Collections.synchronizedSet(new HashSet<QueryFavourites>());

		List<Callable<Void>> tasks = new ArrayList<>(3);
		final AtomicInteger executedCount = new AtomicInteger(0);
		for (int i = 0; i < 3; i++) {
			tasks.add(new Callable<Void>() {

				@Override
				public Void call() throws Exception {
					for (int i = 0; i < repeatInTest; i++) {
						QueryFavourites readFavs = favService.read();
						favs.add(readFavs);

						QueryEntry fav = new QueryEntry(new CqlQuery(CqlQueryType.SELECT,
								"select * from MyTable2 where id=" + UUID.randomUUID()), 5000 + i, 34);
						int retry = 0;
						while (!readFavs.addWithSizeCheck(fav)) {
							retry++;
							assertTrue(retry < 100);

							ImmutableSortedSet<QueryEntry> favsSorted = readFavs.copyAsSortedSet();
							UnmodifiableIterator<QueryEntry> iterator = favsSorted.iterator();
							QueryEntry toRemove = null;
							for (int a = 0; a < favsSorted.size(); a++) {
								toRemove = iterator.next();
							}
							readFavs.remove(toRemove);
						}

						verifyHistEntry(readFavs, fav);

						favService.store(readFavs);
						if (i % 20 == 0) {
							asyncFileStore.flush();
						}

						QueryFavourites readHist = favService.read();
						verifyHistEntry(readHist, fav);

						executedCount.incrementAndGet();
						assertEquals(0, storage.getLockRetryCount());
					}
					return null;
				}

				void verifyHistEntry(QueryFavourites favs, QueryEntry fav) {
					assertNotNull(favs);
					assertTrue("Starred (" + executedCount + "):" + fav + " not found in: " + favs, favs.contains(fav));

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
		return favs;
	}

}

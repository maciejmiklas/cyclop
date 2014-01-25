package org.cyclop.service.history.impl;

import org.cyclop.AbstractTestCase;
import org.cyclop.model.*;
import org.junit.Before;
import org.junit.Test;

import javax.inject.Inject;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;
import java.util.concurrent.*;
import java.util.concurrent.atomic.AtomicInteger;

import static junit.framework.Assert.assertFalse;
import static junit.framework.Assert.assertNotSame;
import static junit.framework.Assert.assertNull;
import static junit.framework.Assert.assertSame;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

/**
 * @author Maciej Miklas
 */
public class TestHistoryService extends AbstractTestCase {

    @Inject
    private HistoryServiceImpl historyService;

    @Inject
    private AsyncFileStore asyncFileStore;

    private UserIdentifier user;

    @Inject
    private FileStorage storage;

    @Before
    public void setup() {
        QueryHistory history = historyService.readHistory();
        assertNotNull(history);
        history.clearHistory();
        history.clearStarred();

        assertEquals(0, history.historySize());
        assertEquals(0, history.starredSize());

        user = historyService.getUser();
        assertNotNull(user);
        assertNotNull(user.id);
    }

    @Test
    public void testCreateAndRead() {
        QueryHistory history = historyService.readHistory();

        for (int i = 0; i < 20; i++) {
            history.addToHistory(new QueryHistoryEntry(new CqlQuery(CqlQueryName.SELECT, "select * from HistoryTest where id=" + i)));
            history.addToStarred(new QueryHistoryEntry(new CqlQuery(CqlQueryName.SELECT, "select * from HistoryStarTest where id=" + i)));

            historyService.store(history);
            QueryHistory historyQueue = asyncFileStore.getFromWriteQueue(user);
            assertNotNull(historyQueue);

            // should be the same instance
            assertSame(history, historyQueue);
        }
        assertEquals(20, history.starredSize());
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
                    assertEquals("select * from HistoryTest where id=" + iterCount, entry.query.cql);
                    assertTrue(readHist + " - " + entry, readHist.containsHistory(entry));
                    assertFalse(readHist + " - " + entry, readHist.containsStarred(entry));
                }
            }
            assertEquals(0, iterCount);
        }

        {
            int iterCount = 20;
            try (QueryHistory.HistoryIterator histIt = history.starredIterator()) {
                while (histIt.hasNext()) {
                    iterCount--;
                    QueryHistoryEntry entry = histIt.next();
                    assertEquals("select * from HistoryStarTest where id=" + iterCount, entry.query.cql);
                    assertTrue(readHist + " - " + entry, readHist.containsStarred(entry));
                    assertFalse(readHist + " - " + entry, readHist.containsHistory(entry));
                }
            }
            assertEquals(0, iterCount);
        }

        {
            history.clearStarred();
            assertEquals(0, history.starredSize());
            historyService.store(history);
            asyncFileStore.flush();
            assertEquals(0, storage.readHistory(user).starredSize());
        }

        {
            history.clearHistory();
            assertEquals(0, history.historySize());
            historyService.store(history);
            asyncFileStore.flush();
            assertEquals(0, storage.readHistory(user).historySize());
        }
    }

    /**
     * HistoryServiceImpl is a session scoped bean. In order to simulate multi-thread access on such beans we have
     * configured ThreadsLimitedScope in testContext.xml
     */
    @Test
    public void testMultiThreadForSingleUser() throws Exception {
        ExecutorService executor = Executors.newFixedThreadPool(5);

        List<Callable<Void>> tasks = new ArrayList<>(3);
        final AtomicInteger executedCount = new AtomicInteger(0);
        for (int i = 0; i < 5; i++) {
            tasks.add(new Callable<Void>() {

                @Override
                public Void call() throws Exception {
                    for (int i = 0; i < 300; i++) {
                        QueryHistory history = historyService.readHistory();
                        QueryHistoryEntry histEntry = new QueryHistoryEntry(new CqlQuery(CqlQueryName.SELECT,
                                "select * from MyTable2 where id=" + UUID.randomUUID()));
                        history.addToHistory(histEntry);

                        QueryHistoryEntry starredEntry = new QueryHistoryEntry(new CqlQuery(CqlQueryName.SELECT,
                                "select * from MyTable2 where id=" + UUID.randomUUID()));
                        history.addToStarred(starredEntry);

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
                            history.containsStarred(starredEntry));

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
        assertEquals(1500, executedCount.get());
    }
}

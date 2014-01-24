package org.cyclop.service.history.impl;

import java.io.File;
import java.util.ArrayList;
import java.util.List;
import java.util.NoSuchElementException;
import java.util.UUID;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;
import javax.inject.Inject;
import org.cyclop.AbstractTestCase;
import org.cyclop.common.AppConfig;
import org.cyclop.model.CqlQuery;
import org.cyclop.model.CqlQueryName;
import org.cyclop.model.QueryHistory;
import org.cyclop.model.QueryHistoryEntry;
import org.cyclop.model.UserIdentifier;
import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

/**
 * @author Maciej Miklas
 */
public class TestHistoryStorage extends AbstractTestCase {

    @Inject
    private AppConfig config;

    @Inject
    private FileStorage storage;

    @Before
    public void init() throws Exception {
        File histFolder = new File(config.history.folder);
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
        File histFolder = new File(config.history.folder);
        histFolder.setWritable(false);

        assertTrue(storage.supported());
        assertFalse(storage.checkSupported());
    }

    @Test
    public void testRead_FileNotFound() throws Exception {
        QueryHistory readHistory = storage.readHistory(new UserIdentifier(UUID.randomUUID()));
        assertNull(readHistory);
    }

    @Test
    public void testEmptyHistory() throws Exception {
        QueryHistory hist = new QueryHistory();
        assertEquals(0, hist.historySize());
        assertEquals(0, hist.starredSize());
        assertFalse(hist.historyIterator().hasNext());
        assertFalse(hist.starredIterator().hasNext());

        CqlQuery query = new CqlQuery(CqlQueryName.SELECT, "select * from MyTable");
        QueryHistoryEntry histEntry = new QueryHistoryEntry(query);
        assertFalse(hist.containsStarred(histEntry));
        assertFalse(hist.containsHistory(histEntry));
        try {
            hist.historyIterator().next();
            fail();
        } catch (NoSuchElementException e) {
            // OK
        }

        try {
            hist.starredIterator().next();
            fail();
        } catch (NoSuchElementException e) {
            // OK
        }

    }

    @Test
    public void testCreateAndRead_SingleHistoryEntry() throws Exception {
        UserIdentifier userId = new UserIdentifier(UUID.randomUUID());
        QueryHistory newHistory = new QueryHistory();

        CqlQuery query = new CqlQuery(CqlQueryName.SELECT, "select * from MyTable");
        QueryHistoryEntry histEntry = new QueryHistoryEntry(query);
        newHistory.addToHistory(histEntry);

        storage.storeHistory(userId, newHistory);

        QueryHistory readHistory = storage.readHistory(userId);
        assertNotNull(readHistory);
        assertEquals(0, readHistory.starredSize());
        assertEquals(1, readHistory.historySize());
        try (QueryHistory.HistoryIterator historyIterator = readHistory.historyIterator()) {
            assertTrue(historyIterator.hasNext());
            QueryHistoryEntry readEntry = historyIterator.next();
            assertEquals(query, readEntry.query);
            assertEquals(histEntry.executedOnUtc.toString(), readEntry.executedOnUtc.toString());

            assertEquals(0, storage.getLockRetryCount());
        }
    }

    @Test
    public void testCreate_EvictStarred() throws Exception {
        UserIdentifier userId = new UserIdentifier(UUID.randomUUID());

        {
            QueryHistory history = new QueryHistory();

            for (int i = 0; i < 70; i++) {
                CqlQuery query = new CqlQuery(CqlQueryName.SELECT, "select * from MyTable where id=" + i);
                history.addToStarred(new QueryHistoryEntry(query));
            }
            assertEquals(50, history.starredSize());

            try (QueryHistory.HistoryIterator hit = history.starredIterator()) {
                for (int i = 49; i >= 0; i--) {
                    assertTrue(hit.hasNext());
                    QueryHistoryEntry entry = hit.next();
                    assertTrue(history.containsStarred(entry));
                    assertNotNull(entry.executedOnUtc);
                    assertEquals("select * from MyTable where id=" + (i + 20), entry.query.cql);
                }
            }

            storage.storeHistory(userId, history);
        }
        {
            QueryHistory history = storage.readHistory(userId);
            try (QueryHistory.HistoryIterator hit = history.starredIterator()) {
                for (int i = 49; i >= 0; i--) {
                    assertTrue(hit.hasNext());
                    QueryHistoryEntry entry = hit.next();
                    assertTrue(history.containsStarred(entry));
                    assertNotNull(entry.executedOnUtc);
                    assertEquals("select * from MyTable where id=" + (i + 20), entry.query.cql);
                }
            }
        }
    }

    @Test
    public void testMove() {

        QueryHistory history = new QueryHistory();

        for (int i = 0; i < 20; i++) {
            history.addToHistory(new QueryHistoryEntry(new CqlQuery(CqlQueryName.SELECT, "select * from MyTable1 " + "where id="
                    + i)));
            history.addToStarred(new QueryHistoryEntry(new CqlQuery(CqlQueryName.SELECT, "select * from MyTable2 " + "where id="
                    + i)));
        }
        assertEquals(20, history.historySize());
        assertEquals(20, history.starredSize());

        {
            QueryHistoryEntry toMoveHist = new QueryHistoryEntry(new CqlQuery(CqlQueryName.SELECT,
                    "select * from MyTable1 where id=10"));
            assertTrue(history.containsHistory(toMoveHist));
            assertFalse(history.containsStarred(toMoveHist));

            history.moveToStarred(toMoveHist);
            assertFalse(history.containsHistory(toMoveHist));
            assertTrue(history.containsStarred(toMoveHist));

            history.moveToHistory(toMoveHist);
            assertTrue(history.containsHistory(toMoveHist));
            assertFalse(history.containsStarred(toMoveHist));
        }

        {
            QueryHistoryEntry toMoveStarred = new QueryHistoryEntry(new CqlQuery(CqlQueryName.SELECT,
                    "select * from MyTable2 where id=14"));
            assertFalse(history.containsHistory(toMoveStarred));
            assertTrue(history.containsStarred(toMoveStarred));

            history.moveToStarred(toMoveStarred);
            assertFalse(history.containsHistory(toMoveStarred));
            assertTrue(history.containsStarred(toMoveStarred));

            history.moveToHistory(toMoveStarred);
            history.moveToHistory(toMoveStarred);
            assertTrue(history.containsHistory(toMoveStarred));
            assertFalse(history.containsStarred(toMoveStarred));
        }
    }

    @Test
    public void testCreate_EvictHistory() throws Exception {
        UserIdentifier userId = new UserIdentifier(UUID.randomUUID());

        {
            QueryHistory history = new QueryHistory();

            for (int i = 0; i < 600; i++) {
                CqlQuery query = new CqlQuery(CqlQueryName.SELECT, "select * from MyTable1 where id=" + i);
                history.addToHistory(new QueryHistoryEntry(query));
            }
            assertEquals(500, history.historySize());

            try (QueryHistory.HistoryIterator hit = history.historyIterator()) {
                for (int i = 499; i >= 0; i--) {
                    assertTrue(hit.hasNext());
                    QueryHistoryEntry entry = hit.next();
                    assertTrue(history.containsHistory(entry));
                    assertNotNull(entry.executedOnUtc);
                    assertEquals("select * from MyTable1 where id=" + (i + 100), entry.query.cql);
                }
            }

            storage.storeHistory(userId, history);
        }

        {
            QueryHistory history = storage.readHistory(userId);
            assertNotNull(history);
            assertEquals(500, history.historySize());
            try (QueryHistory.HistoryIterator hit = history.historyIterator()) {
                for (int i = 499; i > 0; i--) {
                    assertTrue(hit.hasNext());
                    QueryHistoryEntry entry = hit.next();
                    assertTrue(history.containsHistory(entry));
                    assertNotNull(entry.executedOnUtc);
                    assertEquals("select * from MyTable1 where id=" + (i + 100), entry.query.cql);
                }
            }

            for (int i = 0; i < 10; i++) {
                CqlQuery query = new CqlQuery(CqlQueryName.SELECT, "select * from MyTable2 where id=" + i);
                history.addToHistory(new QueryHistoryEntry(query));
            }
            storage.storeHistory(userId, history);
        }

        {
            QueryHistory history = storage.readHistory(userId);
            assertNotNull(history);
            assertEquals(500, history.historySize());

            try (QueryHistory.HistoryIterator hit = history.historyIterator()) {
                for (int i = 499; i > 489; i--) {
                    assertTrue(hit.hasNext());
                    QueryHistoryEntry entry = hit.next();
                    assertTrue(history.containsHistory(entry));
                    assertNotNull(entry.executedOnUtc);
                    assertEquals("select * from MyTable2 where id=" + (i - 490), entry.query.cql);
                }
                for (int i = 489; i > 0; i--) {
                    assertTrue(hit.hasNext());
                    QueryHistoryEntry entry = hit.next();
                    assertTrue(history.containsHistory(entry));
                    assertNotNull(entry.executedOnUtc);
                    assertEquals("select * from MyTable1 where id=" + (i + 110), entry.query.cql);
                }
            }
        }
    }

    @Ignore
    @Test
    public void testMultiThreadForSingleUser() throws Exception {
        ExecutorService executor = Executors.newFixedThreadPool(10);
        final UserIdentifier userId = new UserIdentifier();
        final QueryHistory history = new QueryHistory();

        List<Callable<Void>> tasks = new ArrayList<>(100);
        final AtomicInteger executedCount = new AtomicInteger(0);
        for (int i = 0; i < 2; i++) {
            tasks.add(new Callable<Void>() {

                @Override
                public Void call() throws Exception {
                    for (int i = 0; i < 500; i++) {
                        QueryHistoryEntry histEntry = new QueryHistoryEntry(new CqlQuery(CqlQueryName.SELECT,
                                "select * from MyTable2 where id=" + UUID.randomUUID()));
                        history.addToHistory(histEntry);

                        QueryHistoryEntry starredEntry = new QueryHistoryEntry(new CqlQuery(CqlQueryName.SELECT,
                                "select * from MyTable2 where id=" + UUID.randomUUID()));
                        history.addToStarred(starredEntry);

                        verifyHistEntry(history, histEntry, starredEntry);

                        storage.storeHistory(userId, history);

                        verifyHistEntry(history, histEntry, starredEntry);

                        QueryHistory readHist = storage.readHistory(userId);
                        verifyHistEntry(history, histEntry, starredEntry);
                        verifyHistEntry(readHist, histEntry, starredEntry);

                        executedCount.incrementAndGet();
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
        assertEquals(500, executedCount.get());
    }

}

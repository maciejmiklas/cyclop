package org.cyclop.service.history.impl;

import org.cyclop.AbstractTestCase;
import org.cyclop.common.AppConfig;
import org.cyclop.model.*;
import org.junit.Before;
import org.junit.Test;

import javax.inject.Inject;
import java.io.File;
import java.util.NoSuchElementException;
import java.util.UUID;

import static org.junit.Assert.*;

/**
 * @author Maciej Miklas
 */
public class TestFileStorage extends AbstractTestCase {

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


}

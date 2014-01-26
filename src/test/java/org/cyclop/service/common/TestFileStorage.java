package org.cyclop.service.common;

import com.google.common.collect.ImmutableSortedSet;
import org.cyclop.AbstractTestCase;
import org.cyclop.common.AppConfig;
import org.cyclop.model.*;
import org.cyclop.service.common.FileStorage;
import org.joda.time.DateTime;
import org.junit.Before;
import org.junit.Test;

import javax.inject.Inject;
import java.io.File;
import java.util.Iterator;
import java.util.NoSuchElementException;
import java.util.Set;
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
        assertEquals(0, hist.favouritesSize());
        assertFalse(hist.historyIterator().hasNext());
        assertFalse(hist.copyOfFavourites().iterator().hasNext());

        CqlQuery query = new CqlQuery(CqlQueryName.SELECT, "select * from MyTable");
        QueryHistoryEntry histEntry = new QueryHistoryEntry(query);
        assertFalse(hist.containsFavourite(histEntry));
        assertFalse(hist.containsHistory(histEntry));
        try {
            hist.historyIterator().next();
            fail();
        } catch (NoSuchElementException e) {
            // OK
        }

        try {
            hist.copyOfFavourites().iterator().next();
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
        assertEquals(0, readHistory.favouritesSize());
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
    public void testLimitFavourites() throws Exception {
        UserIdentifier userId = new UserIdentifier(UUID.randomUUID());
        QueryHistory history = new QueryHistory();
        {
            for (int i = 0; i < 50; i++) {
                CqlQuery query = new CqlQuery(CqlQueryName.SELECT, "select * from MyTable where id=" + i);
                assertTrue(history.addToFavouritesWithSizeCheck(new QueryHistoryEntry(query)));
            }
            assertEquals(50, history.favouritesSize());
        }

        checkFavLimitContent(history);

        {
            for (int i = 0; i < 10; i++) {
                CqlQuery query = new CqlQuery(CqlQueryName.SELECT, "select * from MyTableA where id=" + i);
                assertFalse(history.addToFavouritesWithSizeCheck(new QueryHistoryEntry(query)));
            }
            checkFavLimitContent(history);
        }

        {
            for (int i = 0; i < 20; i++) {
                CqlQuery query = new CqlQuery(CqlQueryName.SELECT, "select * from MyTable where id=" + i);
                assertTrue(history.addToFavouritesWithSizeCheck(new QueryHistoryEntry(query)));
            }
            checkFavLimitContent(history);
        }

        {
            DateTime executed = new DateTime(2040, 3, 4, 11, 22);
            QueryHistoryEntry entry = new QueryHistoryEntry(new CqlQuery(CqlQueryName.SELECT,
                    "select * from MyTable where id=13"), executed);

            assertFalse(history.copyOfFavourites().contains(entry));
            assertFalse(history.containsHistory(entry));
            assertTrue(history.containsFavourite(entry));

            history.addToHistory(entry);

            assertTrue(history.containsHistory(entry));
            assertTrue(history.containsFavourite(entry));
            ImmutableSortedSet<QueryHistoryEntry> favs = history.copyOfFavourites();
            assertTrue(favs.contains(entry));
            QueryHistoryEntry firstFav = favs.iterator().next();
            assertEquals(entry, firstFav);
            assertEquals(entry.executedOnUtc, firstFav.executedOnUtc);
        }

        {
            storage.storeHistory(userId, history);

            QueryHistory readHistory = storage.readHistory(userId);
            checkFavLimitContent(readHistory);
            checkFavLimitContent(history);
        }
    }

    private void checkFavLimitContent(QueryHistory history) {
        Set<QueryHistoryEntry> fav = history.copyOfFavourites();
        Iterator<QueryHistoryEntry> favIt = fav.iterator();
        assertEquals(50, fav.size());
        assertEquals(50, history.favouritesSize());
        for (int i = 0; i < 50; i++) {
            assertTrue(favIt.hasNext());
            CqlQuery query = new CqlQuery(CqlQueryName.SELECT, "select * from MyTable where id=" + i);
            assertTrue(history.containsFavourite(new QueryHistoryEntry(query)));
            assertTrue(history.containsFavourite(favIt.next()));
        }
    }


    @Test
    public void testEvictHistory() throws Exception {
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

package org.cyclop.service.history.impl;

import org.cyclop.AbstractTestCase;
import org.cyclop.common.AppConfig;
import org.cyclop.model.CqlQuery;
import org.cyclop.model.CqlQueryName;
import org.cyclop.model.QueryHistoryEntry;
import org.cyclop.model.UserIdentifier;
import org.junit.Before;
import org.junit.Test;
import org.springframework.test.annotation.Repeat;

import javax.inject.Inject;
import java.io.File;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.UUID;

import static org.junit.Assert.*;

/**
 * @author Maciej Miklas
 */
public class TestHistoryStorage extends AbstractTestCase {

    SimpleDateFormat sid = new SimpleDateFormat("yyyy-MM-dd HH:mm");

    @Inject
    private AppConfig config;

    @Inject
    private HistoryStorage storage;

    private final static int REPEAT = 5;

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
    @Repeat(REPEAT)
    public void testCreateAndRead_SingleHistoryEntry() throws Exception {
        UserIdentifier userId = new UserIdentifier(UUID.randomUUID());
        QueryHistory newHistory = new QueryHistory(10, 10);

        Date histDate = new Date();
        CqlQuery query = new CqlQuery(CqlQueryName.SELECT, "select * from MyTable");
        QueryHistoryEntry histEntry = new QueryHistoryEntry(query, histDate);
        newHistory.history.add(histEntry);

        storage.storeHistory(newHistory, userId);

        QueryHistory readHistory = storage.readHistory(userId);
        assertNotNull(readHistory);
        assertEquals(0, readHistory.starred.size());
        assertEquals(1, readHistory.history.size());
        QueryHistoryEntry readEntry = readHistory.history.peek();
        assertEquals(query, readEntry.query);
        assertEquals(sid.format(histDate), sid.format(readEntry.executedOn));

    }
}

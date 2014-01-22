package org.cyclop.service.history.impl;

import java.io.File;
import java.sql.Date;
import java.util.UUID;
import javax.inject.Inject;
import org.cyclop.AbstractTestCase;
import org.cyclop.common.AppConfig;
import org.cyclop.model.CqlQuery;
import org.cyclop.model.CqlQueryName;
import org.cyclop.model.QueryHistoryEntry;
import org.cyclop.model.UserIdentifier;
import org.junit.Before;
import org.junit.Test;
import org.springframework.test.annotation.Repeat;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

/**
 * @author Maciej Miklas
 */
public class TestHistoryStorage extends AbstractTestCase {

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

        Date histDate = new Date(System.currentTimeMillis());
        QueryHistoryEntry histEntry = new QueryHistoryEntry(new CqlQuery(CqlQueryName.SELECT, "select * from mytable"), histDate);
        newHistory.history.add(histEntry);

        storage.storeHistory(newHistory, userId);

        QueryHistory readHistory = storage.readHistory(userId);
        System.out.println("");
    }
}

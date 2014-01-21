package org.cyclop.service.history.impl;

import org.cyclop.AbstractTestCase;
import org.cyclop.common.AppConfig;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import javax.inject.Inject;
import java.io.File;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

/**
 * @author Maciej Miklas
 */
public class TestHistoryService extends AbstractTestCase {

    @Inject
    private AppConfig config;

    @Inject
    private HistoryServiceImpl historyService;

    @Before
    public void init() throws Exception {
        File histFolder = new File(config.history.folder);
        histFolder.setWritable(true);
        assertTrue("History folder not writable:" + histFolder, histFolder.canWrite());
    }

    @After
    public void cleanup() throws Exception {

    }

    @Test
    public void testCheckSupported_CanWrite() throws Exception {
        assertTrue(historyService.supported());
        assertTrue(historyService.checkSupported());
    }

    @Test
    public void testCheckSupported_CanNotWrite() throws Exception {
        File histFolder = new File(config.history.folder);
        histFolder.setWritable(false);

        assertTrue(historyService.supported());
        assertFalse(historyService.checkSupported());
    }
}

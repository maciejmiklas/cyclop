package org.cyclop.test;

import com.datastax.driver.core.Session;
import java.io.File;
import java.io.IOException;
import java.nio.file.FileSystems;
import java.nio.file.Files;
import java.nio.file.Path;
import org.apache.cassandra.io.util.FileUtils;
import org.junit.BeforeClass;
import org.junit.runner.RunWith;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import org.springframework.test.context.web.WebAppConfiguration;

import static org.junit.Assert.assertNotNull;

/**
 * @author Maciej Miklas
 */
@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(locations = {TestProps.TEST_CONTEXT})
@WebAppConfiguration
public abstract class AbstractTestCase {

    private static final EmbeddedCassandra CASSANDRA = new EmbeddedCassandra();

    private static boolean INIT_EXECUTED = false;

    @BeforeClass
    public static void staticInit() throws Exception {
        if (INIT_EXECUTED) {
            return;
        }
        INIT_EXECUTED = true;
        setupHistory();
        setupCassandra();
    }

    private static void setupHistory() throws Exception {
        Path tempPath = FileSystems.getDefault().getPath("target", "cyclop-history-test");
        rmdir(tempPath);
        Files.createDirectory(tempPath);
        System.getProperties().setProperty("history.folder", tempPath.toString());
    }

    private static void setupCassandra() throws Exception {
        CASSANDRA.start();
    }

    private static void rmdir(Path dir) throws IOException {
        File dirFile = dir.toFile();
        if (dirFile.exists()) {
            FileUtils.deleteRecursive(dirFile);
        }
    }

    public Session getCassandraSession() {
        assertNotNull("Cassandra session is null", CASSANDRA.getSession());
        return CASSANDRA.getSession();
    }
}

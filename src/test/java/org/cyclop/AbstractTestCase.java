package org.cyclop;

import org.junit.BeforeClass;
import org.junit.runner.RunWith;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import org.springframework.test.context.web.WebAppConfiguration;

import java.nio.file.Files;
import java.nio.file.Path;

/**
 * @author Maciej Miklas
 */
@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(locations = {TestProps.TEST_CONTEXT})
@WebAppConfiguration
public abstract class AbstractTestCase {

    @BeforeClass
    public static void staticInit() throws Exception {
        setupHistory();
    }

    private static void setupHistory() throws Exception {
        Path tempPath = Files.createTempDirectory("cyclop-");
        System.getProperties().setProperty("history.folder", tempPath.toString());
    }
}

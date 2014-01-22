package org.cyclop.service.io;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.nio.channels.FileChannel;
import org.junit.Test;

/**
 * @author Maciej Miklas
 */
public class TestBackwardInputStream {

    @Test
    public void testReadFileForward() throws Exception {
        File file = new File("/data/prj_idea12/other/cyclop/src/test/resources/testfiles/words.txt");
        try (FileInputStream in = new FileInputStream(file)) {
            FileChannel channel = in.getChannel();
            Reader reader = new InputStreamReader(in, "UTF-8");
            BufferedReader bufReader = new BufferedReader(reader);
            String line = null;
            do {
                line = bufReader.readLine();
                System.out.println("");
            } while (line != null);
        }
    }


    @Test
    public void testReadFileBackward() throws Exception {
        File file = new File("/data/prj_idea12/other/cyclop/src/test/resources/testfiles/words.txt");
        try (FileInputStream in = new FileInputStream(file)) {
            FileChannel channel = in.getChannel();
            BackwardInputStream bis = new BackwardInputStream(channel);
            Reader reader = new InputStreamReader(bis, "US-ASCII");
            BufferedReader bufReader = new BufferedReader(reader);
            String line = null;
            do {
                line = bufReader.readLine();
                System.out.println("");
            } while (line != null);
        }
    }
}

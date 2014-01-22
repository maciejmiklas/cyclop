package org.cyclop.service.io;

import java.io.IOException;
import java.io.InputStream;
import java.nio.ByteBuffer;
import java.nio.channels.FileChannel;
import org.apache.commons.lang.ArrayUtils;

/**
 * @author Maciej Miklas
 */
public final class BackwardInputStream extends InputStream {

    private FileChannel fileChannel;
    private long position = 0;
    private long fileSize = 0;

    public BackwardInputStream(FileChannel fileChannel) throws IOException {
        this.fileChannel = fileChannel;
        position = fileSize = fileChannel.size();
        fileChannel.position(position);
    }

    @Override
    public int read() throws IOException {
        throw new UnsupportedOperationException("Not implemented");
    }


    @Override
    public int read(byte[] respBuf, int off, int len) throws IOException {
        if (position <= 0) {
            return -1;
        }

        // calculate start and and to read from channel
        long startRead = Math.max(0, position - len);
        int readLength = startRead == 0 ? (int) position : Math.min(len, (int) fileSize);
        position = Math.max(0, startRead - 1);

        // move channel's cursor to start position for reading
        fileChannel.position(position);

        // read into buf
        ByteBuffer buf = ByteBuffer.allocate(readLength);
        fileChannel.read(buf);
        buf.flip();

        // reverse bytes since we are reading from end of the file
        byte[] read = buf.array();
        String aa = new String(read);
        ArrayUtils.reverse(read);
        System.arraycopy(read, 0, respBuf, off, readLength);
        return readLength;
    }

    @Override
    public int read(byte[] buf) throws IOException {
        return read(buf, 0, buf.length);
    }
}

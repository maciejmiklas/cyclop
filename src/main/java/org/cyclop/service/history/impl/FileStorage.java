package org.cyclop.service.history.impl;

import java.io.File;
import java.io.IOException;
import java.nio.ByteBuffer;
import java.nio.CharBuffer;
import java.nio.channels.FileChannel;
import java.nio.channels.FileLock;
import java.nio.channels.FileLockInterruptionException;
import java.nio.channels.OverlappingFileLockException;
import java.nio.charset.Charset;
import java.nio.charset.CharsetDecoder;
import java.nio.charset.CharsetEncoder;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardOpenOption;
import java.util.concurrent.atomic.AtomicInteger;
import javax.annotation.PostConstruct;
import javax.annotation.concurrent.NotThreadSafe;
import javax.inject.Inject;
import javax.inject.Named;
import org.cyclop.common.AppConfig;
import org.cyclop.model.QueryHistory;
import org.cyclop.model.UserIdentifier;
import org.cyclop.model.exception.ServiceException;
import org.cyclop.service.converter.JsonMarshaller;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author Maciej Miklas
 */
@Named
@NotThreadSafe
public class FileStorage {
    private final static Logger LOG = LoggerFactory.getLogger(HistoryServiceImpl.class);

    private ThreadLocal<CharsetEncoder> encoder;

    private ThreadLocal<CharsetDecoder> decoder;

    @Inject
    private AppConfig config;

    @Inject
    private JsonMarshaller jsonMarshaller;

    private boolean supported;

    private final AtomicInteger lockRetryCount = new AtomicInteger(0);

    @PostConstruct
    protected void init() {
        supported = checkSupported();

        encoder = new ThreadLocal<CharsetEncoder>() {
            @Override
            protected CharsetEncoder initialValue() {
                Charset charset = Charset.forName("UTF-8");
                CharsetEncoder decoder = charset.newEncoder();
                return decoder;
            }
        };

        decoder = new ThreadLocal<CharsetDecoder>() {
            @Override
            protected CharsetDecoder initialValue() {
                Charset charset = Charset.forName("UTF-8");
                CharsetDecoder decoder = charset.newDecoder();
                return decoder;
            }
        };
    }

    public boolean supported() {
        return supported;
    }

    protected boolean checkSupported() {
        if (!config.history.enabled) {
            LOG.info("Query history is disabled");
            return false;
        }

        File histFolder = new File(config.history.folder);
        if (!histFolder.exists()) {
            LOG.warn("Query history is enabled, but configured folder does not exists:{}", histFolder);
            return false;
        }

        if (!histFolder.canWrite()) {
            LOG.warn("Query history is enabled, but configured folder is read-only:{}", histFolder);
            return false;
        }

        return true;
    }

    public void storeHistory(UserIdentifier userId, QueryHistory history) throws ServiceException {
        Path histPath = getPath(userId);
        try (FileChannel channel = openForWrite(histPath)) {
            String jsonText = jsonMarshaller.marshal(history);
            ByteBuffer buf = encoder.get().encode(CharBuffer.wrap(jsonText));
            int written = channel.write(buf);
            channel.truncate(written);
        } catch (IOException | SecurityException | IllegalStateException e) {
            throw new ServiceException("Error storing query history in:" + histPath + " - " + e.getMessage(), e);
        }
    }

    public QueryHistory readHistory(UserIdentifier userId) throws ServiceException {
        Path histPath = getPath(userId);
        try (FileChannel channel = openForRead(histPath)) {
            if (channel == null) {
                LOG.debug("History file not found: {}", histPath);
                return null;
            }
            int fileSize = (int) channel.size();
            if (fileSize > config.history.maxFileSize) {
                LOG.info("History file: {} too large: {} - skipping it", histPath, fileSize);
                return new QueryHistory();
            }
            ByteBuffer buf = ByteBuffer.allocate(fileSize);
            channel.read(buf);
            buf.flip();
            String decoded = decoder.get().decode(buf).toString();
            QueryHistory history = jsonMarshaller.unmarshal(QueryHistory.class, decoded);
            return history;
        } catch (IOException | SecurityException | IllegalStateException e) {
            throw new ServiceException("Error reading query history from:" + histPath + " - " + e.getMessage(), e);
        }

    }

    private FileChannel openForWrite(Path histPath) throws IOException {
        FileChannel byteChannel = FileChannel.open(histPath, StandardOpenOption.CREATE,
                StandardOpenOption.TRUNCATE_EXISTING, StandardOpenOption.WRITE);
        byteChannel.force(true);
        FileChannel lockChannel = lock(histPath, byteChannel);
        return lockChannel;
    }

    private FileChannel openForRead(Path histPath) throws IOException {
        File file = histPath.toFile();
        if (!file.exists() || !file.canRead()) {
            LOG.debug("History file not found: " + histPath);
            return null;
        }
        FileChannel byteChannel = FileChannel.open(histPath, StandardOpenOption.READ, StandardOpenOption.WRITE);
        FileChannel lockChannel = lock(histPath, byteChannel);
        return lockChannel;
    }

    private FileChannel lock(Path histPath, FileChannel channel) throws IOException {

        long start = System.currentTimeMillis();
        String lastExMessage = null;
        FileChannel lockChannel = null;
        while (lockChannel == null && System.currentTimeMillis() - start < config.history.lockWaitTimeoutMilis) {
            try {
                FileLock lock = channel.lock();
                lockChannel = lock.channel();
            } catch (FileLockInterruptionException | OverlappingFileLockException e) {
                lockRetryCount.incrementAndGet();
                lastExMessage = e.getMessage();
                LOG.debug("File lock on '{}' cannot be obtained (retrying operation): {}", histPath, lastExMessage);
                try {
                    Thread.sleep(100);
                } catch (InterruptedException e1) {
                    Thread.interrupted();
                }
            }
        }
        if (lockChannel == null) {
            throw new ServiceException("File lock on '" + histPath + "' cannot be obtained: " + lastExMessage);
        }

        return lockChannel;
    }

    private Path getPath(UserIdentifier userId) {
        Path histPath = Paths.get(config.history.folder, "history-" + userId.id + ".json");
        return histPath;
    }

    public int getLockRetryCount() {
        return lockRetryCount.get();
    }

}

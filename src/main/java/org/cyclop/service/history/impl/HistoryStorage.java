package org.cyclop.service.history.impl;

import org.cyclop.common.AppConfig;
import org.cyclop.model.UserIdentifier;
import org.cyclop.model.exception.ServiceException;
import org.cyclop.service.converter.JsonMarshaller;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.annotation.PostConstruct;
import javax.inject.Inject;
import javax.inject.Named;
import java.io.File;
import java.io.IOException;
import java.nio.ByteBuffer;
import java.nio.CharBuffer;
import java.nio.channels.FileChannel;
import java.nio.charset.Charset;
import java.nio.charset.CharsetDecoder;
import java.nio.charset.CharsetEncoder;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardOpenOption;

/**
 * @author Maciej Miklas
 */
@Named
public class HistoryStorage {
    private final static Logger LOG = LoggerFactory.getLogger(HistoryServiceImpl.class);

    private ThreadLocal<CharsetEncoder> encoder;

    private ThreadLocal<CharsetDecoder> decoder;

    @Inject
    private AppConfig config;

    @Inject
    private JsonMarshaller jsonMarshaller;

    private boolean supported;

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

    public void storeHistory(QueryHistory history, UserIdentifier userId) throws ServiceException {
        Path histPath = getPath(userId);
        try (FileChannel channel = openForWrite(histPath)) {
            String jsonText = jsonMarshaller.marshal(history);
            ByteBuffer buf = encoder.get().encode(CharBuffer.wrap(jsonText));
            //buf.flip();
            int written = channel.write(buf);
           // channel.truncate(buf.limit());
        } catch (IOException | SecurityException | IllegalStateException e) {
            throw new ServiceException("Error storing query history in:" + histPath + " - " + e.getMessage(), e);
        }
    }

    public QueryHistory readHistory(UserIdentifier userId) throws ServiceException {
        Path histPath = getPath(userId);
        try (FileChannel channel = openForRead(histPath)) {
            int fileSize = (int) channel.size();
            if (fileSize > config.history.maxFileSize) {
                LOG.info("History file: {} too large: {} - skipping it", histPath, fileSize);
                return new QueryHistory(config.history.historyLimit, config.history.starredLimit);
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
       // byteChannel.lock();
        return byteChannel;
    }

    private FileChannel openForRead(Path histPath) throws IOException {

        File file = histPath.toFile();
        if (!file.exists() || !file.canRead()) {
            LOG.debug("History file not found: " + histPath);
            return null;
        }
        FileChannel byteChannel = FileChannel.open(histPath, StandardOpenOption.READ);
        //byteChannel.lock();
        return byteChannel;
    }

    private Path getPath(UserIdentifier userId) {
        Path histPath = Paths.get(config.history.folder, "history-" + userId.id + ".json");
        return histPath;
    }
}

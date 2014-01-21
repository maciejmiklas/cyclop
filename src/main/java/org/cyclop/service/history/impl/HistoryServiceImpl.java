package org.cyclop.service.history.impl;

import com.google.common.collect.ImmutableList;
import org.cyclop.common.AppConfig;
import org.cyclop.model.CqlQuery;
import org.cyclop.model.QueryHistoryEntry;
import org.cyclop.model.ServiceException;
import org.cyclop.model.UserIdentifier;
import org.cyclop.service.converter.JsonMarshaller;
import org.cyclop.service.history.HistoryService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.annotation.PostConstruct;
import javax.inject.Inject;
import javax.inject.Named;
import java.io.*;
import java.nio.ByteBuffer;
import java.nio.channels.FileChannel;
import java.nio.channels.FileLock;
import java.nio.charset.Charset;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardOpenOption;
import java.util.Date;

/**
 * @author Maciej Miklas
 */
@Named
public class HistoryServiceImpl implements HistoryService {
    private final static Logger LOG = LoggerFactory.getLogger(HistoryServiceImpl.class);

    @Inject
    private AppConfig config;

    @Inject
    private JsonMarshaller jsonMarshaller;

    private boolean supported;

    @PostConstruct
    protected void init() {
        supported = checkSupported();
    }

    @Override
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

    @Override
    public void submitLastQuery(CqlQuery query, UserIdentifier userId) throws ServiceException {
        try (FileChannel channel = openForWrite(userId)) {
            QueryHistoryEntry historyEntry = new QueryHistoryEntry(query, new Date());
            String jsonText = jsonMarshaller.marshal(historyEntry) + "\n";
            byte[] jsonByte = jsonText.getBytes(Charset.forName("UTF-8"));
            ByteBuffer buf = ByteBuffer.allocate(jsonByte.length);
            buf.flip();
            channel.write(buf);
        } catch (IOException | SecurityException e) {
            throw new ServiceException(e.getMessage(), e);
        }
    }

    @Override
    public ImmutableList<QueryHistoryEntry> readHistory(UserIdentifier userId) throws ServiceException {
        ImmutableList.Builder<QueryHistoryEntry> respList = ImmutableList.builder();
        try (FileInputStream in = openForRead(userId)) {
            if (in == null) {
                return ImmutableList.of();
            }
            Reader reader = new InputStreamReader(in, "UTF-8");
            BufferedReader bufReader = new BufferedReader(reader);
            for (int readLine = 1; readLine <= config.history.limit; readLine++) {
                String line = bufReader.readLine();
                LOG.debug("Reading history line({}): {}", readLine, line);
                if (line == null) {
                    break;
                }
                try {
                    QueryHistoryEntry historyEntry = jsonMarshaller.unmarshal(QueryHistoryEntry.class, line);
                    respList.add(historyEntry);
                } catch (ServiceException e) {
                    LOG.warn("Error reading history file: " + e.getMessage());
                    LOG.debug(e.getMessage(), e);
                }
            }

        } catch (IOException | SecurityException e) {
            throw new ServiceException(e.getMessage(), e);
        }

        ImmutableList<QueryHistoryEntry> history = respList.build();
        return history;
    }

    private FileChannel openForWrite(UserIdentifier userId) throws IOException {
        Path histPath = getPath(userId);
        FileChannel byteChannel = FileChannel.open(histPath, StandardOpenOption.CREATE);
        FileLock lock = byteChannel.tryLock();
        lock.close();
        return byteChannel;
    }

    private FileInputStream openForRead(UserIdentifier userId) throws IOException {
        Path histPath = getPath(userId);
        File file = histPath.toFile();
        if (!file.exists() || !file.canRead()) {
            LOG.debug("History file not found: " + histPath);
            return null;
        }
        FileInputStream in = new FileInputStream(file);
        FileChannel channel = in.getChannel();
        channel.tryLock();
        return in;
    }

    private Path getPath(UserIdentifier userId) {
        Path histPath = Paths.get(config.history.folder, "hostory-" + userId.id);
        return histPath;
    }
}

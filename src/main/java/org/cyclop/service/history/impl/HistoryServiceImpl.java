package org.cyclop.service.history.impl;

import java.io.File;
import java.io.IOException;
import java.nio.channels.FileChannel;
import java.nio.channels.FileLock;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardOpenOption;
import java.util.Collections;
import java.util.List;

import javax.annotation.PostConstruct;
import javax.inject.Inject;
import javax.inject.Named;

import org.cyclop.common.AppConfig;
import org.cyclop.model.CqlQuery;
import org.cyclop.model.QueryHistoryEntry;
import org.cyclop.model.ServiceException;
import org.cyclop.model.UserIdentifier;
import org.cyclop.service.history.HistoryService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author Maciej Miklas
 */
@Named
public class HistoryServiceImpl implements HistoryService {
    private final static Logger LOG = LoggerFactory.getLogger(HistoryServiceImpl.class);

    @Inject
    private AppConfig config;

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
    }

    @Override
    public List<QueryHistoryEntry> readHistory(UserIdentifier userId) throws ServiceException {
        try (FileChannel channel = openForRead(userId)) {
            if (channel == null) {
                return Collections.emptyList();
            }

        } catch (IOException | SecurityException e) {
            throw new ServiceException(e.getMessage(), e);
        }

        return null;
    }

    private FileChannel openForWrite(UserIdentifier userId) throws IOException {
        Path histPath = getPath(userId);
        FileChannel byteChannel = FileChannel.open(histPath, StandardOpenOption.CREATE);
        FileLock lock = byteChannel.tryLock();
        lock.close();
        return byteChannel;
    }

    private FileChannel openForRead(UserIdentifier userId) throws IOException {
        Path histPath = getPath(userId);
        if (!Files.exists(histPath)) {
            LOG.debug("History file not found: " + histPath);
            return null;
        }

        FileChannel byteChannel = FileChannel.open(histPath, StandardOpenOption.READ);
        FileLock lock = byteChannel.tryLock();
        lock.close();
        return byteChannel;
    }

    private Path getPath(UserIdentifier userId) {
        Path histPath = Paths.get(config.history.folder, "hostory-" + userId.id);
        return histPath;
    }
}

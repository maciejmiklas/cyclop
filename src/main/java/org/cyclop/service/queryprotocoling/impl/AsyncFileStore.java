package org.cyclop.service.queryprotocoling.impl;

import org.cyclop.model.QueryHistory;
import org.cyclop.model.UserIdentifier;
import org.cyclop.service.common.FileStorage;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.scheduling.annotation.Scheduled;

import javax.annotation.PreDestroy;
import javax.inject.Inject;
import javax.inject.Named;
import java.util.HashMap;
import java.util.Map;

/** @author Maciej Miklas */
@Named
class AsyncFileStore<H> {

	private final static int FLUSH_MILIS = 300000;

	@Inject
	private FileStorage fileStorage;

	private final static Logger LOG = LoggerFactory.getLogger(AsyncFileStore.class);

	private final Map<UserIdentifier, H> diskQueue = new HashMap<>();

	public void store(UserIdentifier identifier, H history) {
		synchronized (diskQueue) {
			diskQueue.put(identifier, history);
		}
	}

	public H getFromWriteQueue(UserIdentifier identifier) {
		synchronized (diskQueue) {
			return diskQueue.get(identifier);
		}
	}

	/**
	 * method must be synchronized to avoid parallel write access on files for single user-id. Second synchronization block
	 * on map ensures short lock time on map, so that {@link #store(UserIdentifier, QueryHistory)} method block time is
	 * reduced
	 */
	@Scheduled(initialDelay = FLUSH_MILIS, fixedDelay = FLUSH_MILIS)
	@PreDestroy
	public synchronized void flush() {
		while (true) {
			UserIdentifier identifier;
			H history;

			// synchronize #historyMap only for short time to not block
			// store(...) function by file operation
			synchronized (diskQueue) {
				if (diskQueue.isEmpty()) {
					LOG.debug("Flush done - no more entries found");
					return;
				}
				identifier = diskQueue.keySet().iterator().next();
				history = diskQueue.remove(identifier);
			}
			fileStorage.store(identifier, history);
		}
	}

}

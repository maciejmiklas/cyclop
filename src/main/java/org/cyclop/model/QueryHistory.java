package org.cyclop.model;

import com.google.common.base.Objects;
import org.apache.commons.collections4.queue.CircularFifoQueue;
import org.cyclop.common.AppConfig;

import javax.annotation.concurrent.NotThreadSafe;
import javax.annotation.concurrent.ThreadSafe;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlTransient;
import javax.xml.bind.annotation.adapters.XmlAdapter;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.NoSuchElementException;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

/**
 * @author Maciej Miklas
 */
@ThreadSafe
@XmlJavaTypeAdapter(QueryHistory.Adapter.class)
public class QueryHistory {

    private final CircularFifoQueue<QueryHistoryEntry> history;

    private final CircularFifoQueue<QueryHistoryEntry> starred;

    private final Lock historyLock = new ReentrantLock();

    private final Lock starredLock = new ReentrantLock();

    public QueryHistory() {
        history = new CircularFifoQueue(AppConfig.get().history.historyLimit);
        starred = new CircularFifoQueue(AppConfig.get().history.starredLimit);
    }

    public void clearHistory() {
        clear(history, historyLock);
    }

    public void clearStarred() {
        clear(starred, starredLock);
    }

    private void clear(CircularFifoQueue<QueryHistoryEntry> queue, Lock lock) {
        lock.lock();
        try {
            queue.clear();
        } finally {
            lock.unlock();
        }
    }

    /**
     * CALL CLOSE ON ITERATOR BECAUSE IT HOLDS READ-LOCK
     */
    public HistoryIterator historyIterator() {
        return new HistoryIterator(historyLock, history);
    }

    /**
     * CALL CLOSE ON ITERATOR BECAUSE IT HOLDS READ-LOCK
     */
    public HistoryIterator starredIterator() {
        return new HistoryIterator(starredLock, starred);
    }

    public void moveToStarred(QueryHistoryEntry entry) {
        move(entry, history, starred);
    }

    public void moveToHistory(QueryHistoryEntry entry) {
        move(entry, starred, history);
    }

    /** Implementation is very slow (2xo(n)) - but it's not being used very often */
    private void move(QueryHistoryEntry entry, CircularFifoQueue<QueryHistoryEntry> from, CircularFifoQueue<QueryHistoryEntry> to) {
        historyLock.lock();
        starredLock.lock();
        try {
            to.add(entry);
            from.remove(entry);
        } finally {
            historyLock.unlock();
            starredLock.unlock();
        }
    }

    public int historySize() {
        return size(history, historyLock);
    }

    public int starredSize() {
        return size(starred, starredLock);
    }

    private int size(CircularFifoQueue<QueryHistoryEntry> queue, Lock lock) {
        lock.lock();
        try {
            return queue.size();
        } finally {
            lock.unlock();
        }
    }

    public boolean containsHistory(QueryHistoryEntry entry) {
        return contains(entry, history, historyLock);
    }

    public boolean containsStarred(QueryHistoryEntry entry) {
        return contains(entry, starred, starredLock);
    }

    /** Implementation is very slow (o(n)) - but it's not being used very often */
    private boolean contains(QueryHistoryEntry entry, CircularFifoQueue<QueryHistoryEntry> queue, Lock lock) {
        lock.lock();
        try {
            return queue.contains(entry);
        } finally {
            lock.unlock();
        }
    }

    public boolean addToHistory(QueryHistoryEntry entry) {
        return add(entry, history, historyLock);
    }

    public boolean addToStarred(QueryHistoryEntry entry) {
        return add(entry, starred, starredLock);
    }

    private boolean add(QueryHistoryEntry entry, CircularFifoQueue<QueryHistoryEntry> queue, Lock lock) {
        lock.lock();
        try {
            return queue.add(entry);
        } finally {
            lock.unlock();
        }
    }

    @XmlRootElement
    @XmlAccessorType(XmlAccessType.FIELD)
    @NotThreadSafe
    public final static class QueryHistoryJaxb {

        private List<QueryHistoryEntry> history;

        private List<QueryHistoryEntry> starred;

        @Override
        public String toString() {
            return Objects.toStringHelper(this).add("history", history).add("starred", starred).toString();
        }
    }

    @Override
    public String toString() {
        historyLock.lock();
        starredLock.lock();
        try {
            return Objects.toStringHelper(this).add("history", history).add("starred", starred).toString();
        } finally {
            historyLock.unlock();
            starredLock.unlock();
        }
    }

    @XmlTransient
    @ThreadSafe
    public final static class Adapter extends XmlAdapter<QueryHistoryJaxb, QueryHistory> {

        @Override
        public QueryHistory unmarshal(QueryHistoryJaxb jaxb) throws Exception {
            if (jaxb == null) {
                return null;
            }

            QueryHistory history = new QueryHistory();
            if (jaxb.history != null) {
                history.history.addAll(jaxb.history);
            }

            if (jaxb.starred != null) {
                history.starred.addAll(jaxb.starred);
            }
            return history;
        }

        @Override
        public QueryHistoryJaxb marshal(QueryHistory histObj) throws Exception {
            if (histObj == null) {
                return null;
            }
            QueryHistoryJaxb jaxb = new QueryHistoryJaxb();

            histObj.historyLock.lock();
            try {
                List<QueryHistoryEntry> historyList = new ArrayList<>(histObj.history.size());
                historyList.addAll(histObj.history);
                jaxb.history = historyList;
            } finally {
                histObj.historyLock.unlock();
            }

            histObj.starredLock.lock();
            try {
                List<QueryHistoryEntry> starredList = new ArrayList<>(histObj.starred.size());
                starredList.addAll(histObj.starred);
                jaxb.starred = starredList;
            } finally {
                histObj.starredLock.unlock();
            }
            return jaxb;
        }
    }

    /**
     * CALL CLOSE ON ITERATOR BECAUSE IT HOLDS READ-LOCK <br>
     * Iterates over history entries from newest to oldest entry (reversed fifo queue)
     */
    @XmlTransient
    @ThreadSafe
    public final class HistoryIterator implements Iterator<QueryHistoryEntry>, AutoCloseable {

        private Lock lock;

        private int position;

        private CircularFifoQueue<QueryHistoryEntry> queue;

        private HistoryIterator(Lock lock, CircularFifoQueue<QueryHistoryEntry> queue) {
            this.queue = queue;
            this.position = queue.size() - 1;
            this.lock = lock;
            this.lock.lock();
        }

        @Override
        public boolean hasNext() {
            return position >= 0;
        }

        @Override
        public QueryHistoryEntry next() {
            if (!hasNext()) {
                throw new NoSuchElementException("Current index is on:" + position);
            }
            return queue.get(position--);
        }

        @Override
        public void remove() {
            throw new UnsupportedOperationException("remove is not implemented");
        }

        @Override
        public void close() {
            if (lock != null) {
                lock.unlock();
            }
        }
    }
}
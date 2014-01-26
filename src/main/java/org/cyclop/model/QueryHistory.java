package org.cyclop.model;

import com.google.common.base.Objects;
import com.google.common.collect.ImmutableSortedSet;
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
import java.util.*;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

/**
 * Favourites are sorted by change date, favourites are queued.
 *
 * @author Maciej Miklas
 */
@ThreadSafe
@XmlJavaTypeAdapter(QueryHistory.Adapter.class)
public class QueryHistory {

    private final CircularFifoQueue<QueryHistoryEntry> history;

    private final Set<QueryHistoryEntry> favourites;

    private final Lock lock = new ReentrantLock();


    public QueryHistory() {
        history = new CircularFifoQueue(AppConfig.get().history.historyLimit);
        favourites = new HashSet<>(AppConfig.get().history.starredLimit);
    }

    public void clearHistory() {
        clear(history);
    }

    public void clearFavourite() {
        clear(favourites);
    }

    private void clear(Collection<QueryHistoryEntry> queue) {
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
        return new HistoryIterator(lock, history);
    }

    public ImmutableSortedSet<QueryHistoryEntry> copyOfFavourites() {
        lock.lock();
        try {
            ImmutableSortedSet.Builder<QueryHistoryEntry> builder = ImmutableSortedSet.naturalOrder();
            builder.addAll(favourites);
            ImmutableSortedSet<QueryHistoryEntry> sortedFav = builder.build();
            return sortedFav;
        } finally {
            lock.unlock();
        }
    }

    public int historySize() {
        return size(history);
    }

    public int favouritesSize() {
        return size(favourites);
    }

    private int size(Collection<QueryHistoryEntry> queue) {
        lock.lock();
        try {
            return queue.size();
        } finally {
            lock.unlock();
        }
    }

    public boolean containsHistory(QueryHistoryEntry entry) {
        return contains(entry, history);
    }

    public boolean containsFavourite(QueryHistoryEntry entry) {
        return contains(entry, favourites);
    }

    /**
     * Implementation is very slow (o(n)) - but it's not being used very often
     */
    private boolean contains(QueryHistoryEntry entry, Collection<QueryHistoryEntry> queue) {
        lock.lock();
        try {
            return queue.contains(entry);
        } finally {
            lock.unlock();
        }
    }

    public void addToHistory(QueryHistoryEntry entry) {
        lock.lock();
        try {
            history.add(entry);
            if (favourites.contains(entry)) {
                favourites.remove(entry);
                favourites.add(entry);
            }
        } finally {
            lock.unlock();
        }
    }

    public boolean removeFavourite(QueryHistoryEntry entry) {
        lock.lock();
        try {
            return favourites.remove(entry);
        } finally {
            lock.unlock();
        }
    }

    /**
     * @return true if add was successful, otherwise false - meaning that size limit is reached.
     *         Already existing elements can be always replaced - update change date
     */
    public boolean addToFavouritesWithSizeCheck(QueryHistoryEntry entry) {
        lock.lock();
        try {
            if (favourites.contains(entry)) {
                favourites.remove(entry);
                favourites.add(entry);
            } else if (favourites.size() >= AppConfig.get().history.starredLimit) {
                return false;
            }
            favourites.add(entry);
            return true;
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
            return Objects.toStringHelper(this).add("history", history).add("favourites", starred).toString();
        }
    }

    @Override
    public String toString() {
        lock.lock();
        try {
            return Objects.toStringHelper(this).add("history", history).add("favourites", favourites).toString();
        } finally {
            lock.unlock();
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
                history.favourites.addAll(jaxb.starred);
            }
            return history;
        }

        @Override
        public QueryHistoryJaxb marshal(QueryHistory histObj) throws Exception {
            if (histObj == null) {
                return null;
            }
            QueryHistoryJaxb jaxb = new QueryHistoryJaxb();

            histObj.lock.lock();
            try {
                List<QueryHistoryEntry> historyList = new ArrayList<>(histObj.history.size());
                historyList.addAll(histObj.history);
                jaxb.history = historyList;
                List<QueryHistoryEntry> starredList = new ArrayList<>(histObj.favourites.size());
                starredList.addAll(histObj.favourites);
                jaxb.starred = starredList;
            } finally {
                histObj.lock.unlock();
            }
            return jaxb;
        }
    }

    /**
     * CALL CLOSE ON ITERATOR BECAUSE IT HOLDS READ-LOCK <br>
     * Iterates over history entries from newest to oldest entry (reversed fifo col)
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

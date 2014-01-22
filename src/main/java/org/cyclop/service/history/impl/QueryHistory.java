package org.cyclop.service.history.impl;

import com.google.common.base.Objects;
import com.google.common.collect.EvictingQueue;
import java.util.ArrayList;
import java.util.List;
import javax.annotation.concurrent.NotThreadSafe;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;
import org.cyclop.model.QueryHistoryEntry;

/**
 * #### QUERY HISTORY IS STORED IN USER SESSION - MULTI-THREAD ACCESS IS POSSIBLE ####
 * 
 * @author Maciej Miklas
 */
@NotThreadSafe
public class QueryHistory {

    public final EvictingQueue<QueryHistoryEntry> history;

    public final EvictingQueue<QueryHistoryEntry> starred;

    public QueryHistory(int historyLimit, int starredLimit) {
        history = EvictingQueue.create(historyLimit);
        starred = EvictingQueue.create(starredLimit);
    }

    public QueryHistory(QueryHistoryJson json, int historyLimit, int starredLimit) {
        this(historyLimit, starredLimit);

        if (json.history != null) {
            history.addAll(json.history);
        }

        if (json.starred != null) {
            starred.addAll(json.starred);
        }
    }

    public synchronized QueryHistoryJson toJson() {
        List<QueryHistoryEntry> historyList = new ArrayList<>(history.size());
        historyList.addAll(history);

        List<QueryHistoryEntry> starredList = new ArrayList<>(starred.size());
        starredList.addAll(starred);
        QueryHistoryJson json = new QueryHistoryJson(historyList, starredList);
        return json;
    }

    /**
     * @author Maciej Miklas
     */
    @XmlRootElement
    @XmlAccessorType(XmlAccessType.FIELD)
    public final static class QueryHistoryJson {

        private List<QueryHistoryEntry> history;

        private List<QueryHistoryEntry> starred;

        public QueryHistoryJson(List<QueryHistoryEntry> history, List<QueryHistoryEntry> starred) {
            this.history = history;
            this.starred = starred;
        }

        @Override
        public String toString() {
            return Objects.toStringHelper(this).add("history", history).add("starred", starred).toString();
        }
    }

    @Override
    public String toString() {
        return Objects.toStringHelper(this).add("history", history).add("starred", starred).toString();
    }
}

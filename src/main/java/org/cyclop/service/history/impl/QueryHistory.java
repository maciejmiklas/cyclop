package org.cyclop.service.history.impl;

import com.google.common.base.Objects;
import com.google.common.collect.EvictingQueue;
import org.cyclop.common.AppConfig;
import org.cyclop.model.QueryHistoryEntry;

import javax.annotation.concurrent.NotThreadSafe;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlTransient;
import javax.xml.bind.annotation.adapters.XmlAdapter;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;
import java.util.ArrayList;
import java.util.List;

/**
 * #### QUERY HISTORY IS STORED IN USER SESSION - MULTI-THREAD ACCESS IS POSSIBLE ####
 *
 * @author Maciej Miklas
 */
@NotThreadSafe
@XmlJavaTypeAdapter(QueryHistory.Adapter.class)
public class QueryHistory {

    public final EvictingQueue<QueryHistoryEntry> history;

    public final EvictingQueue<QueryHistoryEntry> starred;

    public QueryHistory(int historyLimit, int starredLimit) {
        history = EvictingQueue.create(historyLimit);
        starred = EvictingQueue.create(starredLimit);
    }

    @XmlRootElement
    @XmlAccessorType(XmlAccessType.FIELD)
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
        return Objects.toStringHelper(this).add("history", history).add("starred", starred).toString();
    }

    @XmlTransient
    public final static class Adapter extends XmlAdapter<QueryHistoryJaxb, QueryHistory> {

        @Override
        public QueryHistory unmarshal(QueryHistoryJaxb jaxb) throws Exception {
            if (jaxb == null) {
                return null;
            }

            QueryHistory history = new QueryHistory(AppConfig.get().history.historyLimit, AppConfig.get().history.starredLimit);
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
            List<QueryHistoryEntry> historyList = new ArrayList<>(histObj.history.size());
            historyList.addAll(histObj.history);

            List<QueryHistoryEntry> starredList = new ArrayList<>(histObj.starred.size());
            starredList.addAll(histObj.starred);

            QueryHistoryJaxb jaxb = new QueryHistoryJaxb();
            jaxb.history = historyList;
            jaxb.starred = starredList;
            return jaxb;
        }
    }
}

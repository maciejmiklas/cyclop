/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.cyclop.model;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.NoSuchElementException;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

import javax.validation.Valid;
import javax.validation.constraints.NotNull;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlTransient;
import javax.xml.bind.annotation.adapters.XmlAdapter;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

import net.jcip.annotations.NotThreadSafe;
import net.jcip.annotations.ThreadSafe;

import org.apache.commons.collections4.queue.CircularFifoQueue;
import org.cyclop.common.AppConfig;

import com.google.common.base.Objects;
import com.google.common.collect.ImmutableList;

/**
 * Favorites are sorted by change date, history is queued.
 *
 * @author Maciej Miklas
 */
@ThreadSafe
@XmlJavaTypeAdapter(QueryHistory.Adapter.class)
public final class QueryHistory implements Serializable, Synchronizable {

	@NotNull
	@Valid
	private final CircularFifoQueue<QueryEntry> history;

	private final Lock lock = new ReentrantLock();

	@Override
	public Lock getLock() {
		return lock;
	}

	public QueryHistory() {
		history = new CircularFifoQueue<>(AppConfig.get().history.entriesLimit);
	}

	public void clear() {
		lock.lock();
		try {
			history.clear();
		} finally {
			lock.unlock();
		}
	}

	/** CALL CLOSE ON ITERATOR BECAUSE IT HOLDS READ-LOCK */
	public HistoryIterator iterator() {
		return new HistoryIterator(lock, history);
	}

	public ImmutableList<QueryEntry> copyAsList() {
		lock.lock();
		try (HistoryIterator iter = iterator()) {
			return ImmutableList.copyOf(iter);
		} finally {
			lock.unlock();
		}
	}

	public int size() {
		lock.lock();
		try {
			return history.size();
		} finally {
			lock.unlock();
		}
	}

	/** Implementation is very slow (o(n)) - but it's not being used very often */
	public boolean contains(QueryEntry entry) {
		lock.lock();
		try {
			return history.contains(entry);
		} finally {
			lock.unlock();
		}
	}

	public void add(QueryEntry entry) {
		lock.lock();
		try {
			history.add(entry);
		} finally {
			lock.unlock();
		}
	}

	@XmlRootElement
	@XmlAccessorType(XmlAccessType.FIELD)
	@NotThreadSafe
	public final static class QueryHistoryJaxb {

		private List<QueryEntry> history;

		@Override
		public String toString() {
			return Objects.toStringHelper(this).add("history", history).toString();
		}
	}

	@Override
	public String toString() {
		lock.lock();
		try {
			return Objects.toStringHelper(this).add("history", history).toString();
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
				List<QueryEntry> historyList = new ArrayList<>(histObj.history.size());
				historyList.addAll(histObj.history);
				jaxb.history = historyList;

			} finally {
				histObj.lock.unlock();
			}
			return jaxb;
		}
	}

	/**
	 * CALL CLOSE ON ITERATOR BECAUSE IT HOLDS READ-LOCK <br>
	 * Iterates over history entries from newest to oldest entry (reversed fifo
	 * col)
	 */
	@XmlTransient
	@ThreadSafe
	public final static class HistoryIterator implements Iterator<QueryEntry>, AutoCloseable {

		private Lock lock;

		private int position;

		private CircularFifoQueue<QueryEntry> queue;

		private HistoryIterator(Lock lock, CircularFifoQueue<QueryEntry> queue) {
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
		public QueryEntry next() {
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

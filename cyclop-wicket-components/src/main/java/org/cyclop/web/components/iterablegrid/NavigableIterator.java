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
package org.cyclop.web.components.iterablegrid;

import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

import net.jcip.annotations.NotThreadSafe;

/** @author Maciej Miklas */
@NotThreadSafe
final class NavigableIterator<E> implements Iterator<E> {

	private final Iterator<E> wrapped;

	/** contains all elements in order returned by iterator */
	private final List<E> cache;

	/** Contains index of the elements that has been read by #next() */
	private final Set<Integer> read = new HashSet<>();

	private int nextIndex = 0;

	private int count = 0;

	private final int limit;

	public NavigableIterator(Iterator<E> wrapped, int limit, List<E> cache) {
		if (wrapped == null) {
			throw new IllegalArgumentException("Wrapped must not be null");
		}
		this.wrapped = wrapped;
		this.limit = limit;
		this.cache = cache;
	}

	public void prepare(int first, int count) {
		if (first > limit) {
			throw new IllegalArgumentException("Cannot skipp " + first + " elements, because iterator is limited to: "
					+ limit);
		}
		this.nextIndex = first;
		this.count = count;
	}

	/**
	 * @return true if there is more data to be cache for current setup done by {@link #prepare(int, int)}
	 */
	@Override
	public boolean hasNext() {
		if (nextIndex > limit) {
			return false;
		}
		boolean next = (count > 0 && (wrapped.hasNext()) || (nextIndex < cache.size()));
		return next;
	}

	public boolean hasMoreData() {
		return wrapped.hasNext();
	}

	@Override
	public E next() {
		E next;
		if (nextIndex > limit) {
			next = null;
		} else if (nextIndex < cache.size()) {
			next = cache.get(nextIndex);
		} else {
			if (!wrapped.hasNext()) {
				next = null;
			} else {

				// Call on NavigableIterator#prepare(...) could skip some
				// elements, this has to be reflected in iterator position on #wrapper
				iterateToIndex(nextIndex);

				next = wrapped.next();
				cache.add(next);
			}
		}

		if (next != null) {
			read.add(nextIndex);
			count--;
			nextIndex++;
		}
		return next;
	}

	@Override
	public void remove() {
		wrapped.remove();
	}

	public int iterateToIndex(int toIndex) {
		toIndex = Math.min(toIndex, limit);
		int rsize = cache.size();
		if (toIndex <= rsize) {
			return toIndex;
		}

		int index = rsize + 1;
		for (; index < toIndex; index++) {
			if (wrapped.hasNext()) {
				cache.add(wrapped.next());
			} else {
				break;
			}
		}
		return index;
	}

	public int maxSize() {
		return Math.max(read.size(), cache.size());
	}

	public int readSize() {
		return read.size();
	}
}

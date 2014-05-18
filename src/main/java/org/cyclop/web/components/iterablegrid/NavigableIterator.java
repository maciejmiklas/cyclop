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

import net.jcip.annotations.NotThreadSafe;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

/** @author Maciej Miklas */
@NotThreadSafe
class NavigableIterator<E> implements Iterator<E> {

	private final Iterator<E> wrapped;

	private List<E> cache = new ArrayList<>();

	private Set<E> read = new HashSet<>();

	private int nextIndex = 0;

	private int count = 0;

	public NavigableIterator(Iterator<E> wrapped) {
		if (wrapped == null) {
			throw new IllegalArgumentException("Wrapped must not be null");
		}
		this.wrapped = wrapped;
	}

	public void prepare(int first, int count) {
		this.nextIndex = first;
		this.count = count;
	}

	/**
	 * @return true if there is more data to be cache for current setup done by {@link #prepare(int, int)}
	 */
	@Override
	public boolean hasNext() {
		boolean next = count > 0 && (wrapped.hasNext() || nextIndex < cache.size());
		return next;
	}

	public boolean hasMoreData() {
		return wrapped.hasNext();
	}

	@Override
	public E next() {
		E next;
		if (nextIndex < cache.size()) {
			next = cache.get(nextIndex);
		} else {
			if (!wrapped.hasNext()) {
				next = null;
			} else {

				// user could skipp single page, but we have to read all elements from iterator and they have
				// to correspond to index position
				iterateToIndex(nextIndex);

				next = wrapped.next();
				cache.add(next);
			}
		}

		if (next != null) {
			count--;
			nextIndex++;
			read.add(next);
		}
		return next;
	}

	@Override
	public void remove() {
		wrapped.remove();
	}

	public int iterateToIndex(int toIndex) {
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

	public int readSize() {
		return read.size();
	}

	public int maxSize() {
		return cache.size();
	}

}

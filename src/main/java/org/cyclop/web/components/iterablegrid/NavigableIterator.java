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
import java.util.Iterator;
import java.util.List;

/** @author Maciej Miklas */
@NotThreadSafe
class NavigableIterator<E> implements Iterator<E> {

	private final Iterator<E> wrapped;

	private List<E> read = new ArrayList<>();

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

	@Override
	public boolean hasNext() {
		boolean next = count > 0 && (wrapped.hasNext() || nextIndex < read.size());
		return next;
	}

	public boolean reachedEnd() {
		return wrapped.hasNext();
	}

	@Override
	public E next() {
		E next;
		if (nextIndex < read.size()) {
			next = read.get(nextIndex);
		} else {
			if (!wrapped.hasNext()) {
				next = null;
			} else {
				next = wrapped.next();
				read.add(next);
			}
		}

		if (next != null) {
			count--;
			nextIndex++;
		}
		return next;
	}

	@Override
	public void remove() {
		wrapped.remove();
	}

	public int getReadElementsCount() {
		return read.size();
	}

}

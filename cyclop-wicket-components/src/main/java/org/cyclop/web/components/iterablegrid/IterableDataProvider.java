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

import org.apache.wicket.markup.repeater.data.IDataProvider;

import java.util.Iterator;

/** @author Maciej Miklas */
public abstract class IterableDataProvider<T> implements IDataProvider<T> {

	private NavigableIterator<T> iterator;

	private long itemsPerPage;

	private long lastPage = 0;

	private long currentPage = 0;

	/** max amount of elements read from iterator. This prevents out of memory for large iterators. */
	private int elementsLimit = Integer.MAX_VALUE;

	protected IterableDataProvider(long itemsPerPage) {
		this.itemsPerPage = itemsPerPage;
		replaceModel();
	}

	public void setElementsLimit(int elementsLimit) {
		this.elementsLimit = elementsLimit;
	}

	@Override
	public final long size() {
		long size;

		// uer goes back on pager, like now he is on page 5 and clicks on 3
		if (lastPage > currentPage) {
			size = iterator.readSize();

			// there is no more data to be read - we are on last page
		} else if (!iterator.hasMoreData()) {
			size = iterator.maxSize();

			// user is not on page 2 and click on next page - 3. Pager will show link for page 4, but not for
			// page 5 - this will first happen when user clicks on page 4. In order to show link for only
			// following page we have to calculate size right. This is the amount of elements up to
			// current page (3) plus element that can be displayed on next page.
			// If the current page is last page, the link for next page mus not be shown - this is handled by
			//  iterateToIndex(...) - it narrows calculated size to maximum allowed value.
		} else {
			size = iterator.readSize() + itemsPerPage + 1;
			size = iterator.iterateToIndex((int) size + 1) - 1;
		}
		return size;
	}

	void reset() {
		lastPage = 0;
		currentPage = 0;
	}

	public void replaceModel() {
		iterator = new NavigableIterator(iterator(), elementsLimit);
		reset();
	}

	@Override
	public final Iterator<T> iterator(long first, long count) {
		iterator.prepare((int) first, (int) count);
		lastPage = currentPage;
		return iterator;
	}

	void setItemsPerPage(long itemsPerPage) {
		this.itemsPerPage = itemsPerPage;
		reset();
	}

	public boolean hasMoreData() {
		return iterator.hasMoreData();
	}

	void setCurrentPage(long currentPage) {
		this.currentPage = currentPage;
	}

	protected abstract Iterator<T> iterator();
}

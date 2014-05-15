package org.cyclop.web.components.iterablegrid;

import org.apache.wicket.markup.repeater.data.IDataProvider;

import java.util.Iterator;

/** @author Maciej Miklas */
public abstract class IterableDataProvider<T> implements IDataProvider<T> {

	private NavigableIterator<T> iterator;

	private long itemsPerPage;

	private long lastPage = 0;

	private long currentPage = 0;

	protected IterableDataProvider(long itemsPerPage) {
		this.itemsPerPage = itemsPerPage;
	}

	@Override
	public final long size() {
		long size;
		if (iterator == null) {
			size = itemsPerPage + 1;
		} else if (lastPage > currentPage) {
			size = iterator.getReadElementsCount();
		} else {
			size = iterator.getReadElementsCount() + itemsPerPage + 1;
		}
		return size;
	}

	@Override
	public final Iterator<T> iterator(long first, long count) {
		if (iterator == null) {
			iterator = new NavigableIterator(iterator());
		}
		iterator.prepare((int) first, (int) count);
		lastPage = currentPage;
		return iterator;
	}

	public long getItemsPerPage() {
		return itemsPerPage;
	}

	void setItemsPerPage(long itemsPerPage) {
		this.itemsPerPage = itemsPerPage;
	}

	void setCurrentPage(long currentPage) {
		this.currentPage = currentPage;
	}

	protected abstract Iterator<T> iterator();
}

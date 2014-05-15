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
		return wrapped.hasNext() && count > 0;
	}

	@Override
	public E next() {
		E next;
		if (nextIndex < read.size() - 1) {
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

	int getNextIndex() {
		return nextIndex;
	}
}

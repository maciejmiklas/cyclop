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

import org.apache.wicket.markup.html.navigation.paging.IPageable;
import org.apache.wicket.markup.repeater.RefreshingView;
import org.apache.wicket.markup.repeater.data.DataView;
import org.apache.wicket.markup.repeater.data.IDataProvider;
import org.apache.wicket.model.IModel;
import org.apache.wicket.util.lang.Args;

import java.util.Iterator;

// ####################################################################################################################
// This class has been copied from wicket 6.14.0
// Only minimal modifications has been made in order to simplify future updates to new wicket version, those are:
// - DataViewBase extends AbstractPageableView from this package and not the original wicket class
// - DataViewBase is package protected
// ####################################################################################################################

/**
 * Base class for data views.
 * <p/>
 * Data views aim to make it very simple to populate your repeating view from a
 * database by utilizing {@link IDataProvider} to act as an interface between
 * the database and the dataview.
 *
 * @param <T>
 *            Model object type
 * @author Igor Vaynberg (ivaynberg)
 * @see IDataProvider
 * @see DataView
 * @see IPageable
 * @see RefreshingView
 */
abstract class DataViewBase<T> extends AbstractPageableView<T> {
	private static final long serialVersionUID = 1L;

	private final IDataProvider<T> dataProvider;

	/**
	 * @param id
	 *            component id
	 * @param dataProvider
	 *            data provider
	 */
	public DataViewBase(String id, IDataProvider<T> dataProvider) {
		super(id);

		this.dataProvider = Args.notNull(dataProvider, "dataProvider");
	}

	/** @return data provider associated with this view */
	protected final IDataProvider<T> internalGetDataProvider() {
		return dataProvider;
	}

	@Override
	protected final Iterator<IModel<T>> getItemModels(long offset, long count) {
		return new ModelIterator<T>(internalGetDataProvider(), offset, count);
	}

	@Override
	protected final long internalGetItemCount() {
		return internalGetDataProvider().size();
	}

	/** @see org.apache.wicket.markup.repeater.AbstractPageableView#onDetach() */
	@Override
	protected void onDetach() {
		dataProvider.detach();
		super.onDetach();
	}

	/**
	 * Helper class that converts input from IDataProvider to an iterator over
	 * view items.
	 *
	 * @param <T>
	 *            Model object type
	 * @author Igor Vaynberg (ivaynberg)
	 */
	private static final class ModelIterator<T> implements Iterator<IModel<T>> {
		private final Iterator<? extends T> items;

		private final IDataProvider<T> dataProvider;

		private final long max;

		private long index;

		/**
		 * Constructor
		 *
		 * @param dataProvider
		 *            data provider
		 * @param offset
		 *            index of first item
		 * @param count
		 *            max number of items to return
		 */
		public ModelIterator(IDataProvider<T> dataProvider, long offset, long count) {
			this.dataProvider = dataProvider;
			max = count;

			items = count > 0 ? dataProvider.iterator(offset, count) : null;
		}

		/** @see Iterator#remove() */
		@Override
		public void remove() {
			throw new UnsupportedOperationException();
		}

		/** @see Iterator#hasNext() */
		@Override
		public boolean hasNext() {
			return items != null && items.hasNext() && (index < max);
		}

		/** @see Iterator#next() */
		@Override
		public IModel<T> next() {
			index++;
			return dataProvider.model(items.next());
		}
	}
}

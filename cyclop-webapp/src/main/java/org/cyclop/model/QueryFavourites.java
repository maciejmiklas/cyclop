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

import com.google.common.base.Objects;
import com.google.common.collect.ImmutableSortedSet;
import net.jcip.annotations.NotThreadSafe;
import net.jcip.annotations.ThreadSafe;
import org.cyclop.common.AppConfig;

import javax.validation.Valid;
import javax.validation.constraints.NotNull;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlTransient;
import javax.xml.bind.annotation.adapters.XmlAdapter;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

/**
 * Favorites are sorted by change date, history is queued.
 *
 * @author Maciej Miklas
 */
@ThreadSafe
@XmlJavaTypeAdapter(QueryFavourites.Adapter.class)
public final class QueryFavourites implements Serializable, Synchronizable {

	@NotNull
	@Valid
	private final Set<QueryEntry> favourites;

	private final Lock lock = new ReentrantLock();

	public QueryFavourites() {
		favourites = new HashSet<>(AppConfig.get().favourites.entriesLimit);
	}

	@Override
	public Lock getLock() {
		return lock;
	}

	public void clear() {
		lock.lock();
		try {
			favourites.clear();
		} finally {
			lock.unlock();
		}
	}

	public ImmutableSortedSet<QueryEntry> copyAsSortedSet() {
		lock.lock();
		try {
			ImmutableSortedSet.Builder<QueryEntry> builder = ImmutableSortedSet.naturalOrder();
			builder.addAll(favourites);
			ImmutableSortedSet<QueryEntry> sortedFav = builder.build();
			return sortedFav;
		} finally {
			lock.unlock();
		}
	}

	public int size() {
		lock.lock();
		try {
			return favourites.size();
		} finally {
			lock.unlock();
		}
	}

	/** Implementation is very slow (o(n)) - but it's not being used very often */
	public boolean contains(QueryEntry entry) {
		lock.lock();
		try {
			return favourites.contains(entry);
		} finally {
			lock.unlock();
		}
	}

	public boolean remove(QueryEntry entry) {
		lock.lock();
		try {
			return favourites.remove(entry);
		} finally {
			lock.unlock();
		}
	}

	/**
	 * @return true if add was successful, otherwise false - meaning that readSize limit is reached. Already existing elements
	 *         can be always replaced - update change date
	 */
	public boolean addWithSizeCheck(QueryEntry entry) {
		lock.lock();
		try {
			if (favourites.contains(entry)) {
				favourites.remove(entry);
				favourites.add(entry);
			} else if (favourites.size() >= AppConfig.get().favourites.entriesLimit) {
				return false;
			}
			favourites.add(entry);
			return true;
		} finally {
			lock.unlock();
		}
	}

	@XmlRootElement
	@XmlAccessorType(XmlAccessType.FIELD)
	@NotThreadSafe
	public final static class FavouritesJaxb {

		private List<QueryEntry> favourites;

		@Override
		public String toString() {
			return Objects.toStringHelper(this).add("favourites", favourites).toString();
		}
	}

	@Override
	public String toString() {
		lock.lock();
		try {
			return Objects.toStringHelper(this).add("favourites", favourites).toString();
		} finally {
			lock.unlock();
		}
	}

	@XmlTransient
	@ThreadSafe
	public final static class Adapter extends XmlAdapter<FavouritesJaxb, QueryFavourites> {

		@Override
		public QueryFavourites unmarshal(FavouritesJaxb jaxb) throws Exception {
			if (jaxb == null) {
				return null;
			}

			QueryFavourites favs = new QueryFavourites();
			if (jaxb.favourites != null) {
				favs.favourites.addAll(jaxb.favourites);
			}
			return favs;
		}

		@Override
		public FavouritesJaxb marshal(QueryFavourites favObj) throws Exception {
			if (favObj == null) {
				return null;
			}
			FavouritesJaxb jaxb = new FavouritesJaxb();

			favObj.lock.lock();
			try {
				List<QueryEntry> starredList = new ArrayList<>(favObj.favourites.size());
				starredList.addAll(favObj.favourites);
				jaxb.favourites = starredList;
			} finally {
				favObj.lock.unlock();
			}
			return jaxb;
		}
	}

}

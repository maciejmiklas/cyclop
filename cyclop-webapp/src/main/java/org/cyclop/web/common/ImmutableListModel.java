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
package org.cyclop.web.common;

import java.util.ArrayList;
import java.util.List;

import org.apache.wicket.model.util.GenericBaseModel;

import com.google.common.collect.ImmutableList;

/** @author Maciej Miklas */
public class ImmutableListModel<T> extends GenericBaseModel<ImmutableList<? extends T>> {

	private final List<ModelChangeListener<T>> listeners = new ArrayList<>();

	public ImmutableListModel() {
		ImmutableList<? extends T> empty = ImmutableList.of();
		setObject(empty);
	}

	public ImmutableListModel(ImmutableList<T> data) {
		setObject(data);
	}

	@Override
	protected final ImmutableList<? extends T> createSerializableVersionOf(ImmutableList<? extends T> object) {
		return object;
	}

	public void registerOnChangeListener(ModelChangeListener<T> listener) {
		listeners.add(listener);
	}

	@Override
	public void setObject(ImmutableList<? extends T> object) {
		super.setObject(object);

		for (ModelChangeListener<T> list : listeners) {
			list.onModelChanged(object);
		}
	}

	@FunctionalInterface
	public interface ModelChangeListener<T> {
		void onModelChanged(ImmutableList<? extends T> object);
	}
}

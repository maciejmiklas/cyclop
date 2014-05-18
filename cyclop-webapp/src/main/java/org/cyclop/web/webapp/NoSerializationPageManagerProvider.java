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
package org.cyclop.web.webapp;

import org.apache.wicket.Application;
import org.apache.wicket.DefaultPageManagerProvider;
import org.apache.wicket.page.IManageablePage;
import org.apache.wicket.pageStore.IDataStore;
import org.apache.wicket.pageStore.IPageStore;

import java.io.Serializable;

/** @author Maciej Miklas */
public class NoSerializationPageManagerProvider extends DefaultPageManagerProvider {

	public NoSerializationPageManagerProvider(Application application) {
		super(application);
	}

	@Override
	protected IPageStore newPageStore(IDataStore dataStore) {
		return new IPageStore() {
			@Override
			public void destroy() {
			}

			@Override
			public IManageablePage getPage(String sessionId, int pageId) {
				return null;
			}

			@Override
			public void removePage(String sessionId, int pageId) {
			}

			@Override
			public void storePage(String sessionId, IManageablePage page) {
			}

			@Override
			public void unbind(String sessionId) {
			}

			@Override
			public Serializable prepareForSerialization(String sessionId, Object page) {
				return null;
			}

			@Override
			public Object restoreAfterSerialization(Serializable serializable) {
				return null;
			}

			@Override
			public IManageablePage convertToPage(Object page) {
				return null;
			}
		};

	}
}

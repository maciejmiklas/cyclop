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
package org.cyclop.web.components.pagination;

import org.apache.wicket.markup.html.navigation.paging.IPageable;
import org.apache.wicket.model.IModel;

import java.io.Serializable;

/** @author Maciej Miklas */
class NavigationIncrementLinkCssModel implements IModel<String>, Serializable {

	protected final IPageable pageable;

	private final long pageNumber;

	public NavigationIncrementLinkCssModel(IPageable pageable, long pageNumber) {
		this.pageable = pageable;
		this.pageNumber = pageNumber;
	}

	@Override
	public String getObject() {
		return isEnabled() ? "" : "disabled";
	}

	@Override
	public void setObject(String object) {
	}

	@Override
	public void detach() {
	}

	public boolean isEnabled() {
		boolean enabled;
		if (pageNumber < 0) {
			enabled = !isFirst();
		} else {
			enabled = !isLast();
		}
		return enabled;
	}

	public boolean isFirst() {
		return pageable.getCurrentPage() <= 0;
	}

	public boolean isLast() {
		return pageable.getCurrentPage() >= (pageable.getPageCount() - 1);
	}
}

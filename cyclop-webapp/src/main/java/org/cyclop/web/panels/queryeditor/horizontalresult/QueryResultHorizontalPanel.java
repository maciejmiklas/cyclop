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
package org.cyclop.web.panels.queryeditor.horizontalresult;

import org.apache.wicket.model.IModel;
import org.cyclop.model.CqlQueryResult;
import org.cyclop.web.components.pagination.BootstrapPagingNavigator;
import org.cyclop.web.panels.queryeditor.QueryResultPanel;

/** @author Maciej Miklas */
public class QueryResultHorizontalPanel extends QueryResultPanel {

	public QueryResultHorizontalPanel(String id, IModel<CqlQueryResult> model) {
		super(id, model);
	}

	@Override
	protected BootstrapPagingNavigator initPagingProvider() {
		// TODO Auto-generated method stub
		return null;
	}

}

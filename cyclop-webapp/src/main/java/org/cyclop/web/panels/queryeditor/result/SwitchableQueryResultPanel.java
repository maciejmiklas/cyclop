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
package org.cyclop.web.panels.queryeditor.result;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.markup.html.panel.Panel;
import org.apache.wicket.model.IModel;
import org.cyclop.model.CqlQueryResult;
import org.cyclop.web.panels.queryeditor.result.horizontal.QueryResultHorizontalPanel;
import org.cyclop.web.panels.queryeditor.result.vertical.QueryResultVerticalPanel;

/** @author Maciej Miklas */
public class SwitchableQueryResultPanel extends Panel {

	private final IModel<CqlQueryResult> model;
	private ViewType type;
	private QueryResultPanel queryResultPanel;

	public SwitchableQueryResultPanel(String id, IModel<CqlQueryResult> model, ViewType type) {
		super(id);
		this.model = model;
		this.type = type;
	}

	@Override
	protected void onInitialize() {
		super.onInitialize();

		queryResultPanel = type == ViewType.HORIZONTAL ? new QueryResultHorizontalPanel("queryResultPanel", model)
				: new QueryResultVerticalPanel("queryResultPanel", model);

		add(queryResultPanel);
	}

	public void switchView(AjaxRequestTarget target, ViewType type) {
		this.type = type;

		remove(queryResultPanel);

		queryResultPanel = queryResultPanel
				.createFromTemplate(type == ViewType.HORIZONTAL ? QueryResultHorizontalPanel.class
						: QueryResultVerticalPanel.class);
		add(queryResultPanel);

		target.add(this);
		QueryResultPanel.appendTableJs(target);
	}

	@Override
	protected void onModelChanged() {
		super.onModelChanged();
		queryResultPanel.modelChanged();
	}

	public static enum ViewType {
		VERTICAL, HORIZONTAL;

		public static ViewType fromOrientation(int orientation) {
			ViewType type = null;
			switch (orientation) {
			case 1:
				type = HORIZONTAL;
				break;
			default:
			case 2:
				type = VERTICAL;
				break;
			}
			return type;
		}
	}
}

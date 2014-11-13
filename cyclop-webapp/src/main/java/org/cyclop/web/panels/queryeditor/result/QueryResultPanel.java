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

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.util.Iterator;
import java.util.List;
import java.util.Optional;

import javax.inject.Inject;

import org.apache.wicket.Component;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.markup.head.IHeaderResponse;
import org.apache.wicket.markup.head.JavaScriptHeaderItem;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.navigation.paging.IPageableItems;
import org.apache.wicket.markup.html.panel.Panel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.PropertyModel;
import org.apache.wicket.request.resource.JavaScriptResourceReference;
import org.cyclop.common.AppConfig;
import org.cyclop.model.CqlExtendedColumnName;
import org.cyclop.model.CqlPartitionKey;
import org.cyclop.model.CqlQueryResult;
import org.cyclop.model.CqlRowMetadata;
import org.cyclop.model.UserPreferences;
import org.cyclop.model.exception.ServiceException;
import org.cyclop.service.um.UserManager;
import org.cyclop.web.common.TransientModel;
import org.cyclop.web.components.column.WidgetFactory;
import org.cyclop.web.components.iterablegrid.IterableDataProvider;
import org.cyclop.web.components.pagination.BootstrapPagingNavigator;
import org.cyclop.web.components.pagination.PagerConfigurator;

import com.datastax.driver.core.Row;
import com.google.common.collect.ImmutableList;

/** @author Maciej Miklas */
public abstract class QueryResultPanel extends Panel {

	private static final JavaScriptResourceReference JS_REF = new JavaScriptResourceReference(QueryResultPanel.class,
			"queryResultPanel.js");

	protected final static String EMPTYVAL = "-";
	private final RowDataProvider rowDataProvider;

	private final IModel<CqlQueryResult> queryResultModel;

	private final ColumnsModel columnsModel;

	private Label cqlResultText;

	private final CqlResultTextModel cqlResultTextModel;

	private WebMarkupContainer resultTable;

	protected final AppConfig config = AppConfig.get();

	private BootstrapPagingNavigator pager;

	@Inject
	protected UserManager um;

	@Inject
	protected WidgetFactory widgetFactory;

	private boolean showResultsTableOnInit = false;
	private long initPage = 0;

	public QueryResultPanel(String id, IModel<CqlQueryResult> model) {
		this(id, model, Optional.empty());
	}

	public QueryResultPanel(String id, IModel<CqlQueryResult> model, Optional<RowDataProvider> rowDataProvider) {
		super(id, model);
		setRenderBodyOnly(true);
		this.queryResultModel = model;
		this.rowDataProvider = rowDataProvider.orElse(new RowDataProvider());
		columnsModel = new ColumnsModel();
		cqlResultTextModel = new CqlResultTextModel();
	}

	public QueryResultPanel createFromTemplate(Class<? extends QueryResultPanel> panelClass) {

		try {
			Constructor<? extends QueryResultPanel> constructor = panelClass.getConstructor(String.class, IModel.class,
					Optional.class);
			QueryResultPanel resPan = constructor.newInstance(getId(), queryResultModel, Optional.of(rowDataProvider));
			resPan.showResultsTableOnInit = true;
			resPan.initPage = pager.getCurrentPage();
			return resPan;
		} catch (NoSuchMethodException | SecurityException | InstantiationException | IllegalAccessException
				| IllegalArgumentException | InvocationTargetException e) {
			throw new ServiceException("Cannot create QueryResultPanel instance: " + e.getMessage(), e);
		}

	}

	@Override
	protected final void onInitialize() {
		super.onInitialize();
		rowDataProvider.setElementsLimit(config.queryEditor.rowsLimit);
		cqlResultText = initClqReslutText();
		resultTable = initResultsTable();

		IModel<CqlRowMetadata> metadataModel = PropertyModel.of(queryResultModel, "rowMetadata");
		IPageableItems pagable = initTableHeader(resultTable, columnsModel, rowDataProvider, metadataModel);
		pager = createPager(pagable);

		if (showResultsTableOnInit) {
			blendInResultsTable();
		}
	}

	protected Component createRowKeyColumn(String wid, Row row, IModel<CqlRowMetadata> metadataModel) {
		CqlPartitionKey partitionKey = metadataModel.getObject().partitionKey;

		Component component;
		if (partitionKey != null) {
			component = widgetFactory.createColumnValue(row, Optional.of(partitionKey), partitionKey, wid);
		} else {
			component = new Label(wid, EMPTYVAL);
		}
		return component;
	}

	@Override
	protected final void onModelChanged() {
		super.onModelChanged();
		if (queryResultModel.getObject().isEmpty()) {
			showCqlResultText("Query executed successfully, result is empty");
		} else {
			showResultsTable();
		}
	}

	protected abstract IPageableItems initTableHeader(WebMarkupContainer resultTable, ColumnsModel columnsModel,
			RowDataProvider rowDataProvider, IModel<CqlRowMetadata> metadataModel);

	protected void hideResultsTable() {
		resultTable.setVisible(false);
		columnsModel.clean();
	}

	private void showCqlResultText(String text) {
		hideResultsTable();
		cqlResultText.setVisible(true);
		cqlResultTextModel.setObject(text);
	}

	private void showResultsTable() {
		hideCqlResultText();
		blendInResultsTable();
		pager.reset();
		rowDataProvider.replaceModel();
	}

	private void blendInResultsTable() {
		resultTable.setVisible(true);
		columnsModel.updateResult(queryResultModel.getObject().rowMetadata);
	}

	private void hideCqlResultText() {
		cqlResultText.setVisible(false);
		cqlResultTextModel.clean();
	}

	private WebMarkupContainer initResultsTable() {
		WebMarkupContainer resultTable = new WebMarkupContainer("resultTable");
		resultTable.setOutputMarkupPlaceholderTag(true);
		resultTable.setVisible(false);
		add(resultTable);
		return resultTable;
	}

	private Label initClqReslutText() {
		Label cqlResultText = new Label("cqlResultText", cqlResultTextModel);
		cqlResultText.setVisible(false);
		cqlResultText.setOutputMarkupPlaceholderTag(true);
		add(cqlResultText);
		return cqlResultText;
	}

	protected final static class CqlResultTextModel implements IModel<String> {
		private String label = "";

		public void clean() {
			this.label = "";
		}

		@Override
		public void detach() {
		}

		@Override
		public String getObject() {
			return label;
		}

		@Override
		public void setObject(String label) {
			this.label = label;
		}
	}

	protected final static class ColumnsModel implements IModel<List<CqlExtendedColumnName>> {
		private CqlRowMetadata result;

		private List<CqlExtendedColumnName> content = ImmutableList.of();

		public ColumnsModel() {
			this.content = ImmutableList.of();
		}

		public void clean() {
			this.content = ImmutableList.of();
			this.result = CqlRowMetadata.EMPTY;
		}

		@Override
		public void detach() {
		}

		@Override
		public List<CqlExtendedColumnName> getObject() {
			return content;
		}

		@Override
		public void setObject(List<CqlExtendedColumnName> object) {
			content = object;
		}

		public CqlRowMetadata getResult() {
			return result;
		}

		public void updateResult(CqlRowMetadata result) {
			this.result = result;
			setObject(result.columns);
		}
	}

	private BootstrapPagingNavigator createPager(IPageableItems pageable) {
		BootstrapPagingNavigator pager = new BootstrapPagingNavigator("rowsPager", pageable, new PagerConfigurator() {

			@Override
			public void onItemsPerPageChanged(AjaxRequestTarget target, long newItemsPerPage) {
				UserPreferences prefs = um.readPreferences().setPagerEditorItems(newItemsPerPage);
				um.storePreferences(prefs);
				appendTableJs(target);
			}

			@Override
			public long getInitialItemsPerPage() {
				return um.readPreferences().getPagerEditorItems();
			}
		}) {
			@Override
			protected void onAjaxEvent(AjaxRequestTarget target) {
				super.onAjaxEvent(target);
				appendTableJs(target);
			}
		};
		resultTable.add(pager);
		pager.setCurrentPage(initPage);
		return pager;
	}

	public static void appendTableJs(AjaxRequestTarget target) {
		target.appendJavaScript("initRwdTable();");
	}

	public static void initTableResizeJs(IHeaderResponse response) {
		response.render(JavaScriptHeaderItem.forReference(JS_REF));
	}

	public final class RowDataProvider extends IterableDataProvider<Row> {

		protected RowDataProvider() {
			super(um.readPreferences().getPagerEditorItems());
		}

		@Override
		protected Iterator<Row> iterator() {
			CqlQueryResult res = queryResultModel.getObject();
			return res.iterator();
		}

		@Override
		public IModel<Row> model(Row row) {
			return new TransientModel<Row>(row);
		}

		@Override
		public void detach() {
		}
	}
}

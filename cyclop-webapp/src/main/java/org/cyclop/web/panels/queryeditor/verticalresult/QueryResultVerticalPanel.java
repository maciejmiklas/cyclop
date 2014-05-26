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
package org.cyclop.web.panels.queryeditor.verticalresult;

import com.datastax.driver.core.Row;
import com.google.common.collect.ImmutableList;
import org.apache.wicket.Component;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.injection.Injector;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.list.ListItem;
import org.apache.wicket.markup.html.list.ListView;
import org.apache.wicket.markup.html.panel.Panel;
import org.apache.wicket.markup.repeater.Item;
import org.apache.wicket.model.IModel;
import org.cyclop.model.CqlExtendedColumnName;
import org.cyclop.model.CqlPartitionKey;
import org.cyclop.model.CqlQuery;
import org.cyclop.model.CqlQueryResult;
import org.cyclop.model.CqlRowMetadata;
import org.cyclop.model.UserPreferences;
import org.cyclop.service.cassandra.QueryService;
import org.cyclop.service.um.UserManager;
import org.cyclop.web.common.TransientModel;
import org.cyclop.web.components.column.WidgetFactory;
import org.cyclop.web.components.iterablegrid.IterableDataProvider;
import org.cyclop.web.components.iterablegrid.IterableGridView;
import org.cyclop.web.components.pagination.BootstrapPagingNavigator;
import org.cyclop.web.components.pagination.PagerConfigurator;

import javax.inject.Inject;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

/** @author Maciej Miklas */
public class QueryResultVerticalPanel extends Panel {

	private final RowDataProvider rowDataProvider;

	private final ColumnsModel columnsModel;

	private final WebMarkupContainer resultTable;

	private final Label cqlResultText;

	private final CqlResultTextModel cqlResultTextModel;

	private final RowsModel rowsModel;

	@Inject
	private UserManager um;

	@Inject
	private QueryService queryService;

	@Inject
	private WidgetFactory widgetFactory;

	private BootstrapPagingNavigator pager;

	public QueryResultVerticalPanel(String id) {
		super(id);
		Injector.get().inject(this);
		setOutputMarkupPlaceholderTag(true);

		cqlResultTextModel = new CqlResultTextModel();
		columnsModel = new ColumnsModel();
		rowDataProvider = new RowDataProvider();

		cqlResultText = new Label("cqlResultText", cqlResultTextModel);
		cqlResultText.setVisible(false);
		cqlResultText.setOutputMarkupPlaceholderTag(true);
		add(cqlResultText);

		resultTable = new WebMarkupContainer("resultTable");
		resultTable.setOutputMarkupPlaceholderTag(true);
		resultTable.setVisible(false);
		add(resultTable);

		rowsModel = initRowNamesList(resultTable, rowDataProvider);
		initColumnList(resultTable, columnsModel, rowsModel);
	}

	public CqlQueryResult executeQuery(CqlQuery query, AjaxRequestTarget target) {
		target.add(this);
		CqlQueryResult result;
		try {
			result = queryService.execute(query);
		} catch (Exception e) {
			showCqlResultText("CQL error: " + e.getMessage());
			return null;
		}

		if (!result.iterator().hasNext()) {
			showCqlResultText("Query executed successfully, result is empty");
		} else {
			showResultsTable(result);
		}
		return result;
	}

	private void hideCqlResultText() {
		cqlResultText.setVisible(false);
		cqlResultTextModel.clean();
	}

	private void hideResultsTable() {
		resultTable.setVisible(false);
		rowDataProvider.clean();
		columnsModel.clean();
		rowsModel.getObject().clear();
	}

	private void initColumnList(WebMarkupContainer resultTable, ColumnsModel columnsModel, final RowsModel rowsModel) {
		ListView<CqlExtendedColumnName> columnList = new ListView<CqlExtendedColumnName>("columnList", columnsModel) {
			@Override
			protected void populateItem(ListItem<CqlExtendedColumnName> item) {
				final CqlExtendedColumnName columnName = item.getModelObject();

				WebMarkupContainer columnListRow = new WebMarkupContainer("columnListRow");
				item.add(columnListRow);

				Label columnNameLabel = new Label("columnName", columnName.part);
				columnListRow.add(columnNameLabel);

				ColumnsModel model = (ColumnsModel) getModel();
				CqlRowMetadata result = model.getResult();
				final CqlPartitionKey partitionKey = result == null ? null : result.partitionKey;

				ListView<Row> columnValueList = new ListView<Row>("columnValueList", rowsModel) {

					@Override
					protected void populateItem(ListItem<Row> item) {
						Row row = item.getModelObject();

						Component component = widgetFactory
								.createForColumn(row, partitionKey, columnName, "columnValue");
						item.add(component);
						component.setRenderBodyOnly(true);
					}
				};
				columnListRow.add(columnValueList);
			}
		};
		resultTable.add(columnList);
	}

	private RowsModel initRowNamesList(WebMarkupContainer resultTable, final RowDataProvider rowDataProvider) {

		final List<Row> displayedRows = new ArrayList<>();
		IterableGridView<Row> rowNamesList = new IterableGridView<Row>("rowNamesList", rowDataProvider) {

			@Override
			protected void onBeforeRender() {
				displayedRows.clear();
				super.onBeforeRender();
			}

			@Override
			protected void populateEmptyItem(Item<Row> item) {
				item.add(new Label("rowName", ""));
			}

			@Override
			protected void populateItem(Item<Row> item) {
				Row row = item.getModel().getObject();
				displayedRows.add(row);
				CqlQueryResult result = rowDataProvider.result;
				CqlPartitionKey partitionKey = result.rowMetadata.partitionKey;

				Component component;
				if (partitionKey != null) {
					component = widgetFactory.createForColumn(row, partitionKey, partitionKey, "rowName");
				} else {
					component = new Label("rowName", displayedRows.size());
				}

				item.add(component);
			}
		};
		resultTable.add(rowNamesList);
		rowNamesList.setColumns(1);

		pager = new BootstrapPagingNavigator("rowNamesListPager", rowNamesList, new PagerConfigurator() {

			@Override
			public void onItemsPerPageChanged(AjaxRequestTarget target, long newItemsPerPage) {
				UserPreferences prefs = um.readPreferences().setPagerEditorItems(newItemsPerPage);
				um.storePreferences(prefs);
			}

			@Override
			public long getInitialItemsPerPage() {
				return um.readPreferences().getPagerEditorItems();
			}
		});
		resultTable.add(pager);

		return new RowsModel(displayedRows);
	}

	private void showCqlResultText(String text) {
		hideResultsTable();
		cqlResultText.setVisible(true);
		cqlResultTextModel.setObject(text);
	}

	private void showResultsTable(CqlQueryResult result) {
		hideCqlResultText();
		pager.reset();
		resultTable.setVisible(true);
		rowDataProvider.replaceModel(result);
	}

	private final static class ColumnsModel implements IModel<List<CqlExtendedColumnName>> {
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

		private CqlRowMetadata getResult() {
			return result;
		}

		public void updateResult(CqlRowMetadata result) {
			this.result = result;
			setObject(result.columns);
		}
	}

	private final static class CqlResultTextModel implements IModel<String> {
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

	private final static class RowsModel implements IModel<List<Row>> {

		private List<Row> content;

		public RowsModel(List<Row> content) {
			this.content = content;
		}

		@Override
		public void detach() {
		}

		@Override
		public List<Row> getObject() {
			return content;
		}

		@Override
		public void setObject(List<Row> object) {
			content = object;
		}

	}

	private final class RowDataProvider extends IterableDataProvider<Row> {

		private CqlQueryResult result;

		protected RowDataProvider() {
			super(um.readPreferences().getPagerEditorItems());
		}

		public void replaceModel(CqlQueryResult result) {
			this.result = result;
			replaceModel();
		}

		@Override
		protected Iterator<Row> iterator() {
			CqlQueryResult res = result == null ? CqlQueryResult.EMPTY : result;
			columnsModel.updateResult(res.rowMetadata);
			return res.iterator();
		}

		@Override
		public IModel<Row> model(Row row) {
			return new TransientModel(row);
		}

		@Override
		public void detach() {
		}

		public void clean() {
			result = null;
		}
	}
}

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
package org.cyclop.web.panels.queryeditor.result.horizontal;

import java.util.Optional;

import javax.inject.Inject;

import org.apache.wicket.Component;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.list.ListItem;
import org.apache.wicket.markup.html.list.ListView;
import org.apache.wicket.markup.html.navigation.paging.IPageableItems;
import org.apache.wicket.markup.repeater.Item;
import org.apache.wicket.model.IModel;
import org.cyclop.model.CqlExtendedColumnName;
import org.cyclop.model.CqlPartitionKey;
import org.cyclop.model.CqlPartitionKeyValue;
import org.cyclop.model.CqlQueryResult;
import org.cyclop.model.CqlRowMetadata;
import org.cyclop.service.converter.DataExtractor;
import org.cyclop.web.components.iterablegrid.IterableGridView;
import org.cyclop.web.panels.queryeditor.result.QueryResultPanel;

import com.datastax.driver.core.Row;

/** @author Maciej Miklas */
public final class QueryResultHorizontalPanel extends QueryResultPanel {

	@Inject
	private DataExtractor extractor;

	public QueryResultHorizontalPanel(String id, IModel<CqlQueryResult> model) {
		super(id, model);
	}

	public QueryResultHorizontalPanel(String id, IModel<CqlQueryResult> model, Optional<RowDataProvider> rowDataProvider) {
		super(id, model, rowDataProvider);
	}

	@Override
	protected IPageableItems initTableHeader(WebMarkupContainer resultTable, ColumnsModel columnsModel,
			RowDataProvider rowDataProvider, IModel<CqlRowMetadata> metadataModel) {

		initColumnList(resultTable, columnsModel);
		IPageableItems rowsList = initRowsList(resultTable, columnsModel, rowDataProvider, metadataModel);
		return rowsList;
	}

	private void initColumnList(WebMarkupContainer resultTable, ColumnsModel columnsModel) {
		ListView<CqlExtendedColumnName> columnList = new ListView<CqlExtendedColumnName>("columnList", columnsModel) {
			@Override
			protected void populateItem(ListItem<CqlExtendedColumnName> item) {
				final CqlExtendedColumnName columnName = item.getModelObject();
				Label columnNameLabel = new Label("columnName", columnName.part);
				item.add(columnNameLabel);
			}
		};
		resultTable.add(columnList);
	}

	protected IPageableItems initRowsList(WebMarkupContainer resultTable, ColumnsModel columnsModel,
			RowDataProvider rowDataProvider, IModel<CqlRowMetadata> metadataModel) {

		IterableGridView<Row> rowsList = new IterableGridView<Row>("rowsList", rowDataProvider) {

			@Override
			protected void populateEmptyItem(Item<Row> item) {
			}

			@Override
			protected void populateItem(Item<Row> item) {
				populateRowKey(item, metadataModel);
				populateColumnValues(item, metadataModel, columnsModel);
			}
		};
		resultTable.add(rowsList);
		rowsList.setColumns(1);
		return rowsList;
	}

	private void populateRowKey(Item<Row> item, IModel<CqlRowMetadata> metadataModel) {
		Row row = item.getModel().getObject();
		Component rowKey = createRowKeyColumn("rowKey", row, metadataModel);
		item.add(rowKey);
	}

	private void populateColumnValues(Item<Row> item, IModel<CqlRowMetadata> metadataModel, ColumnsModel columnsModel) {
		CqlPartitionKey partitionKey = metadataModel.getObject().partitionKey;
		Row row = item.getModel().getObject();

		Optional<CqlPartitionKeyValue> cqlPartitionKeyValue = partitionKey == null ? Optional.empty() : Optional
				.of(extractor.extractPartitionKey(row, partitionKey));
		Optional<CqlPartitionKey> partitionKeyOpt = Optional.ofNullable(partitionKey);

		ListView<CqlExtendedColumnName> columnValueList = new ListView<CqlExtendedColumnName>("columnValueList",
				columnsModel) {

			@Override
			protected void populateItem(ListItem<CqlExtendedColumnName> item) {
				CqlExtendedColumnName column = item.getModelObject();

				Component component = widgetFactory.createColumnValue(row, partitionKeyOpt, column, "columnValue");
				item.add(component);
				component.setRenderBodyOnly(true);
				widgetFactory.addColumnTitle(item, cqlPartitionKeyValue, column);
			}
		};
		item.add(columnValueList);
	}

}

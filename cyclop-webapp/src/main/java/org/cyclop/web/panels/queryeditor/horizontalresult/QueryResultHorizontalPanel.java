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
import org.cyclop.model.CqlQueryResult;
import org.cyclop.model.CqlRowMetadata;
import org.cyclop.web.components.iterablegrid.IterableGridView;
import org.cyclop.web.panels.queryeditor.QueryResultPanel;

import com.datastax.driver.core.Row;

/** @author Maciej Miklas */
public final class QueryResultHorizontalPanel extends QueryResultPanel {

    public QueryResultHorizontalPanel(String id, IModel<CqlQueryResult> model) {
	super(id, model);
    }

    @Override
    protected IPageableItems init(
	    WebMarkupContainer resultTable,
	    ColumnsModel columnsModel,
	    RowDataProvider rowDataProvider,
	    IModel<CqlRowMetadata> metadataModel) {

	initColumnList(resultTable, columnsModel);
	IPageableItems rowsList = initRowsList(resultTable, columnsModel, rowDataProvider, metadataModel);
	return rowsList;
    }

    private void initColumnList(WebMarkupContainer resultTable, ColumnsModel columnsModel) {
	ListView<CqlExtendedColumnName> columnList = new ListView<CqlExtendedColumnName>(
		"columnList",
		columnsModel) {
	    @Override
	    protected void populateItem(ListItem<CqlExtendedColumnName> item) {
		final CqlExtendedColumnName columnName = item.getModelObject();
		Label columnNameLabel = new Label("columnName", columnName.part);
		item.add(columnNameLabel);
	    }
	};
	resultTable.add(columnList);
    }

    protected IPageableItems initRowsList(
	    WebMarkupContainer resultTable,
	    ColumnsModel columnsModel,
	    RowDataProvider rowDataProvider,
	    IModel<CqlRowMetadata> metadataModel) {

	IterableGridView<Row> rowsList = new IterableGridView<Row>("rowsList", rowDataProvider) {

	    @Override
	    protected void populateEmptyItem(Item<Row> item) {
	    }

	    @Override
	    protected void populateItem(Item<Row> item) {
		Row row = item.getModel().getObject();
		Component rowKey = createRowKeyColumn("rowKey", row, metadataModel);
		item.add(rowKey);

		ListView<CqlExtendedColumnName> columnValueList = new ListView<CqlExtendedColumnName>(
			"columnValueList",
			columnsModel) {

		    @Override
		    protected void populateItem(ListItem<CqlExtendedColumnName> item) {
			CqlExtendedColumnName column = item.getModelObject();

			CqlPartitionKey partitionKey = metadataModel.getObject().partitionKey;
			Component component = widgetFactory.createForColumn(
				row,
				partitionKey,
				column,
				"columnValue");
			item.add(component);
			component.setRenderBodyOnly(true);
		    }
		};
		item.add(columnValueList);
	    }
	};
	resultTable.add(rowsList);
	rowsList.setColumns(1);
	return rowsList;
    }

}

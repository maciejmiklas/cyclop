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
package org.cyclop.web.panels.queryeditor;

import java.util.Collections;
import java.util.Iterator;
import java.util.List;

import javax.inject.Inject;

import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.panel.Panel;
import org.apache.wicket.model.IModel;
import org.cyclop.common.AppConfig;
import org.cyclop.model.CqlExtendedColumnName;
import org.cyclop.model.CqlQueryResult;
import org.cyclop.model.CqlRowMetadata;
import org.cyclop.service.um.UserManager;
import org.cyclop.web.common.TransientModel;
import org.cyclop.web.components.column.WidgetFactory;
import org.cyclop.web.components.iterablegrid.IterableDataProvider;
import org.cyclop.web.components.pagination.BootstrapPagingNavigator;

import com.datastax.driver.core.Row;
import com.google.common.collect.ImmutableList;

/** @author Maciej Miklas */
public abstract class QueryResultPanel extends Panel {

    protected final RowDataProvider rowDataProvider;

    protected final IModel<CqlQueryResult> model;

    protected final ColumnsModel columnsModel;

    protected final RowsModel rowsModel;

    private final Label cqlResultText;

    private final CqlResultTextModel cqlResultTextModel;

    protected final WebMarkupContainer resultTable;

    protected final AppConfig config = AppConfig.get();

    private final BootstrapPagingNavigator pager;

    @Inject
    protected UserManager um;

    @Inject
    protected WidgetFactory widgetFactory;

    public QueryResultPanel(String id, IModel<CqlQueryResult> model) {
	super(id, model);
	this.model = model;

	columnsModel = new ColumnsModel();
	rowsModel = new RowsModel();

	rowDataProvider = new RowDataProvider();
	rowDataProvider.setElementsLimit(config.queryEditor.rowsLimit);

	cqlResultTextModel = new CqlResultTextModel();
	cqlResultText = initClqReslutText();
	resultTable = initResultsTable();
	pager = initPagingProvider();
    }

    @Override
    protected void onModelChanged() {
	super.onModelChanged();
	if (model.getObject().isEmpty()) {
	    showCqlResultText("Query executed successfully, result is empty");
	}
	else {
	    showResultsTable();
	}
    }

    protected abstract BootstrapPagingNavigator initPagingProvider();

    protected void hideResultsTable() {
	resultTable.setVisible(false);
	columnsModel.clean();
	rowsModel.getObject().clear();
    }

    protected void showCqlResultText(String text) {
	hideResultsTable();
	cqlResultText.setVisible(true);
	cqlResultTextModel.setObject(text);
    }

    protected void showResultsTable() {
	hideCqlResultText();
	resultTable.setVisible(true);
	pager.reset();
	rowDataProvider.replaceModel();
	columnsModel.updateResult(model.getObject().rowMetadata);
    }

    protected void hideCqlResultText() {
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

    protected final static class RowsModel implements IModel<List<Row>> {

	private List<Row> content;

	public RowsModel() {
	    this.content = Collections.emptyList();
	}

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

    protected final class RowDataProvider extends IterableDataProvider<Row> {

	protected RowDataProvider() {
	    super(um.readPreferences().getPagerEditorItems());
	}

	@Override
	protected Iterator<Row> iterator() {
	    CqlQueryResult res = model.getObject();
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

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

import javax.inject.Inject;

import org.apache.wicket.Component;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.authroles.authorization.strategies.role.Roles;
import org.apache.wicket.authroles.authorization.strategies.role.annotations.AuthorizeInstantiation;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.panel.Panel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.request.mapper.parameter.PageParameters;
import org.apache.wicket.util.string.StringValue;
import org.cyclop.model.ContextCqlCompletion;
import org.cyclop.model.CqlQuery;
import org.cyclop.model.CqlQueryResult;
import org.cyclop.model.UserPreferences;
import org.cyclop.service.cassandra.QueryService;
import org.cyclop.service.exporter.CsvQueryResultExporter;
import org.cyclop.service.um.UserManager;
import org.cyclop.web.panels.queryeditor.buttons.ButtonListener;
import org.cyclop.web.panels.queryeditor.buttons.ButtonsPanel;
import org.cyclop.web.panels.queryeditor.completionhint.CompletionHintPanel;
import org.cyclop.web.panels.queryeditor.cqlhelp.CqlHelpPanel;
import org.cyclop.web.panels.queryeditor.editor.CompletionChangeListener;
import org.cyclop.web.panels.queryeditor.editor.EditorPanel;
import org.cyclop.web.panels.queryeditor.export.QueryResultExport;
import org.cyclop.web.panels.queryeditor.horizontalresult.QueryResultHorizontalPanel;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/** @author Maciej Miklas */
@AuthorizeInstantiation(Roles.ADMIN)
public class QueryEditorPanel extends Panel {

    private final static Logger LOG = LoggerFactory.getLogger(QueryEditorPanel.class);

    private final CqlHelpPanel cqlHelpPanel;

    private final CompletionHintPanel cqlCompletionHintPanel;

    private boolean queryRunning = false;

    private CqlQuery lastQuery;

    private final QueryResultExport queryResultExport;

    @Inject
    private CsvQueryResultExporter exporter;

    @Inject
    private UserManager userManager;

    @Inject
    private QueryService queryService;

    private final IModel<CqlQueryResult> queryResultModel = Model.of(CqlQueryResult.EMPTY);

    private final Label queryErrorLabel;

    public QueryEditorPanel(String id, PageParameters params) {
	super(id);
	setRenderBodyOnly(true);

	cqlHelpPanel = new CqlHelpPanel("cqlHelp");
	add(cqlHelpPanel);

	cqlCompletionHintPanel = new CompletionHintPanel("cqlInfoHint", "Completion Hint");
	add(cqlCompletionHintPanel);

	Panel queryResultPanel = initQueryResultPanel();
	EditorPanel queryEditorPanel = initQueryEditorPanel(params);

	UserPreferences preferences = userManager.readPreferences();
	boolean completionEnabled = preferences.isShowCqlCompletionHint();
	cqlCompletionHintPanel.setVisible(completionEnabled);
	initButtons(queryEditorPanel, queryResultPanel, completionEnabled);

	queryResultExport = new QueryResultExport(this, exporter);

	queryErrorLabel = initQueryErrorLabel();
    }

    private Label initQueryErrorLabel() {
	Label queryErrorLabel = new Label("queryError", Model.of(""));
	add(queryErrorLabel);
	queryErrorLabel.setVisible(false);
	queryErrorLabel.setOutputMarkupPlaceholderTag(true);
	return queryErrorLabel;
    }

    private Panel initQueryResultPanel() {
	Panel queryResultVerticalPanel = new QueryResultHorizontalPanel("queryResult", queryResultModel);
	// Panel queryResultVerticalPanel = new
	// QueryResultVerticalPanel("queryResulta", queryResultModel);
	queryResultVerticalPanel.setOutputMarkupPlaceholderTag(true);
	queryResultVerticalPanel.setOutputMarkupId(true);
	add(queryResultVerticalPanel);
	return queryResultVerticalPanel;
    }

    private EditorPanel initQueryEditorPanel(PageParameters params) {

	StringValue editorContentVal = params.get("cql");
	String editorContent = editorContentVal == null ? null : editorContentVal.toString();

	EditorPanel queryEditorPanel = new EditorPanel("queryEditorPanel", editorContent);
	add(queryEditorPanel);
	queryEditorPanel.setOutputMarkupPlaceholderTag(true);
	queryEditorPanel.setOutputMarkupId(true);

	queryEditorPanel.registerCompletionChangeListener(new CompletionChangeHelp());
	queryEditorPanel.registerCompletionChangeListener(new CompletionChangeHint());
	return queryEditorPanel;
    }

    private ButtonsPanel initButtons(
	    final EditorPanel editorPanel,
	    final Panel queryResultPanel,
	    boolean completionEnabled) {
	ButtonListener buttonListener = new ButtonListener() {

	    @Override
	    public void onClickQueryResultExport(AjaxRequestTarget target) {
		queryResultExport.initiateDownload(target, lastQuery);
	    }

	    @Override
	    public void onClickExecCql(AjaxRequestTarget target) {
		handleExecQuery(target, editorPanel, queryResultPanel);
	    }

	    @Override
	    public void onClickCompletion(AjaxRequestTarget target, boolean pressed) {
		cqlCompletionHintPanel.setVisible(pressed);
		target.add(cqlCompletionHintPanel);
	    }

	    @Override
	    public void onEesultOrientation(AjaxRequestTarget target, int orientation) {
		// TODO Auto-generated method stub

	    }
	};

	ButtonsPanel buttonsPanel = new ButtonsPanel("buttons", buttonListener, completionEnabled);
	add(buttonsPanel);
	return buttonsPanel;
    }

    private void handleExecQuery(AjaxRequestTarget target, EditorPanel editorPanel, Panel queryResultPanel) {

	// this cannot happen, because java script disables execute
	// button - it's DOS prevention
	if (queryRunning) {
	    LOG.warn("Query still running - cannot execute second one");
	    return;
	}

	CqlQuery query = editorPanel.getEditorContent();

	if (query == null) {
	    return;
	}
	queryRunning = true;
	try {
	    CqlQueryResult queryResult = queryService.execute(query);
	    lastQuery = query;
	    queryResultModel.setObject(queryResult);
	    queryResultPanel.modelChanged();
	    queryErrorLabel.setVisible(false);
	    queryResultPanel.setVisible(true);
	}
	catch (Exception e) {
	    queryErrorLabel.setVisible(true);
	    queryResultPanel.setVisible(false);
	    queryErrorLabel.setDefaultModelObject(e.getMessage());
	}
	finally {
	    queryRunning = false;
	}
	editorPanel.resetCompletion();

	target.add(queryErrorLabel);
	target.add(queryResultPanel);
    }

    private final class CompletionChangeHelp implements CompletionChangeListener {

	@Override
	public void onCompletionChange(ContextCqlCompletion currentCompletion) {
	    cqlHelpPanel.changeCompletion(currentCompletion);
	}

	@Override
	public Component getReferencesForRefresh() {
	    return cqlHelpPanel;
	}
    }

    private final class CompletionChangeHint implements CompletionChangeListener {

	@Override
	public void onCompletionChange(ContextCqlCompletion currentCompletion) {
	    cqlCompletionHintPanel.changeCompletion(currentCompletion);
	}

	@Override
	public Component getReferencesForRefresh() {
	    return cqlCompletionHintPanel;
	}
    }

}

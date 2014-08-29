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
package org.cyclop.web.panels.queryeditor.editor;

import java.util.ArrayList;
import java.util.List;
import java.util.SortedSet;

import javax.inject.Inject;

import org.apache.commons.lang.StringUtils;
import org.apache.wicket.Component;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.attributes.AjaxRequestAttributes;
import org.apache.wicket.ajax.form.OnChangeAjaxBehavior;
import org.apache.wicket.markup.head.IHeaderResponse;
import org.apache.wicket.markup.head.JavaScriptReferenceHeaderItem;
import org.apache.wicket.markup.head.OnDomReadyHeaderItem;
import org.apache.wicket.markup.html.form.TextArea;
import org.apache.wicket.markup.html.panel.Panel;
import org.apache.wicket.model.Model;
import org.apache.wicket.request.cycle.RequestCycle;
import org.apache.wicket.request.resource.JavaScriptResourceReference;
import org.cyclop.model.ContextCqlCompletion;
import org.cyclop.model.CqlPart;
import org.cyclop.model.CqlQuery;
import org.cyclop.model.CqlQueryType;
import org.cyclop.service.completion.CompletionService;
import org.cyclop.web.common.JsFunctionBuilder;

/** @author Maciej Miklas */
public class EditorPanel extends Panel {

    private static final JavaScriptResourceReference SUGGEST = new JavaScriptResourceReference(
	    EditorPanel.class,
	    "asuggest.js");

    private final String editorMarkupIdJq;

    private final String editorMarkupIdJs;

    @Inject
    private CompletionService completionService;

    private ContextCqlCompletion currentCompletion;

    private final List<CompletionChangeListener> completionChangeListeners = new ArrayList<>();

    private final TextArea<String> editor;

    public EditorPanel(String id, String editorContent) {
	super(id);

	editor = initEditor(StringUtils.trimToNull(editorContent));
	editorMarkupIdJq = "#" + editor.getMarkupId();
	editorMarkupIdJs = editor.getMarkupId();
    }

    public void registerCompletionChangeListener(CompletionChangeListener list) {
	completionChangeListeners.add(list);
    }

    public void resetCompletion() {
	currentCompletion = null;
    }

    public CqlQuery getEditorContent() {
	String editorValue = editor.getDefaultModelObjectAsString();
	editorValue = StringUtils.trimToNull(editorValue);
	if (editorValue == null || currentCompletion == null) {
	    return null;
	}
	CqlQuery cq = new CqlQuery(currentCompletion.queryName, editorValue);
	return cq;
    }

    private TextArea<String> initEditor(String editorContent) {
	final Model<String> editorModel = new Model<>();
	if (editorContent != null) {
	    editorModel.setObject(editorContent);
	}

	TextArea<String> editor = new TextArea<>("queryEditor", editorModel);
	editor.setEscapeModelStrings(false);

	add(editor);

	editor.add(new OnChangeAjaxBehavior() {

	    @Override
	    protected void onUpdate(AjaxRequestTarget target) {

		Component cmp = getComponent();
		String editorValue = cmp.getDefaultModelObjectAsString();

		ContextCqlCompletion cqlCompletion;
		if (StringUtils.isEmpty(editorValue)) {
		    cqlCompletion = completionService.findInitialCompletion();
		}
		else {
		    RequestCycle requestCycle = RequestCycle.get();
		    int index = requestCycle.getRequest().getRequestParameters()
			    .getParameterValue("cursorPos").toInt();
		    CqlQuery cqlQuery = new CqlQuery(CqlQueryType.UNKNOWN, editorValue);
		    cqlCompletion = completionService.findCompletion(cqlQuery, index);
		}
		if (cqlCompletion.cqlCompletion.isEmpty() || cqlCompletion.equals(currentCompletion)) {
		    return;
		}

		fireCompletionChanged(cqlCompletion);
		String suggestsScript = generateReplaceSuggestsJs(
			editorMarkupIdJq,
			cqlCompletion.cqlCompletion.fullCompletion);
		target.appendJavaScript(suggestsScript);

		for (CompletionChangeListener list : completionChangeListeners) {
		    Component refresh = list.getReferencesForRefresh();
		    if (refresh == null) {
			continue;
		    }
		    target.add(refresh);
		}
	    }

	    @Override
	    protected void updateAjaxAttributes(AjaxRequestAttributes attributes) {
		attributes.getDynamicExtraParameters().add(
			"return {'cursorPos' : getCaretPosition(" + editorMarkupIdJs + ")}");
		super.updateAjaxAttributes(attributes);
	    }
	});
	return editor;
    }

    private void fireCompletionChanged(ContextCqlCompletion currentCompletion) {
	this.currentCompletion = currentCompletion;
	for (CompletionChangeListener list : completionChangeListeners) {
	    list.onCompletionChange(currentCompletion);
	}
    }

    @Override
    public void renderHead(IHeaderResponse response) {
	super.renderHead(response);
	response.render(JavaScriptReferenceHeaderItem.forReference(SUGGEST));

	ContextCqlCompletion cqlCompletion = completionService.findInitialCompletion();
	fireCompletionChanged(cqlCompletion);

	String suggestsScript = generateInitSuggestsJs(
		editorMarkupIdJq,
		cqlCompletion.cqlCompletion.fullCompletion);
	response.render(OnDomReadyHeaderItem.forScript(suggestsScript));
    }

    private String generateInitSuggestsJs(String editorMarkupId, SortedSet<? extends CqlPart> suggestValues) {
	return generateSuggests("initSuggests", editorMarkupId, suggestValues);
    }

    private String generateReplaceSuggestsJs(String editorMarkupId, SortedSet<? extends CqlPart> suggestValues) {
	return generateSuggests("replaceSuggests", editorMarkupId, suggestValues);
    }

    private String generateSuggests(
	    String function,
	    String editorMarkupId,
	    SortedSet<? extends CqlPart> suggestValues) {

	String js = JsFunctionBuilder.function(function).param(editorMarkupId).array(suggestValues).build();
	return js;
    }

}

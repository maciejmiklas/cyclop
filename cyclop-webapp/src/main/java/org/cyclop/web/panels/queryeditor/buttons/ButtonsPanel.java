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
package org.cyclop.web.panels.queryeditor.buttons;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.AjaxFallbackLink;
import org.apache.wicket.markup.head.IHeaderResponse;
import org.apache.wicket.markup.head.JavaScriptReferenceHeaderItem;
import org.apache.wicket.markup.html.panel.Panel;
import org.apache.wicket.request.resource.JavaScriptResourceReference;
import org.cyclop.web.components.buttons.StateButton;

/** @author Maciej Miklas */
public class ButtonsPanel extends Panel {

	private static final JavaScriptResourceReference JS_BUTTONS = new JavaScriptResourceReference(ButtonsPanel.class,
			"buttons.js");

	public ButtonsPanel(String id, final ButtonListener buttonListener, boolean completionPressed) {
		super(id);
		setRenderBodyOnly(true);

		AjaxFallbackLink<Void> addToFavourites = new AjaxFallbackLink<Void>("addToFavourites") {
			@Override
			public void onClick(AjaxRequestTarget target) {
				// buttonListener.onClickExecCql(target);
			}
		};
		add(addToFavourites);
		addToFavourites.setVisible(false);

		AjaxFallbackLink<Void> execQuery = new AjaxFallbackLink<Void>("execQuery") {
			@Override
			public void onClick(AjaxRequestTarget target) {
				buttonListener.onClickExecCql(target);
				target.appendJavaScript("queryExecutedResponse()");
			}
		};
		add(execQuery);

		AjaxFallbackLink<Void> exportQueryResult = new AjaxFallbackLink<Void>("exportQueryResult") {
			@Override
			public void onClick(AjaxRequestTarget target) {
				buttonListener.onClickQueryResultExport(target);
			}
		};
		add(exportQueryResult);

		AjaxFallbackLink<Void> completion = new StateButton("completion", completionPressed, "btn btn-sm btn-primary",
				"btn btn-sm btn-primary active") {
			@Override
			protected void onClick(AjaxRequestTarget target, boolean pressed) {
				buttonListener.onClickCompletion(target, pressed);
			}
		};
		add(completion);
	}

	@Override
	public void renderHead(IHeaderResponse response) {
		super.renderHead(response);
		response.render(JavaScriptReferenceHeaderItem.forReference(JS_BUTTONS));
	}
}

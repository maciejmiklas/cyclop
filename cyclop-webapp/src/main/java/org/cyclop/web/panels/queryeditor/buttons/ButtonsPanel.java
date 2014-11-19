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

import javax.inject.Inject;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.AjaxFallbackLink;
import org.apache.wicket.ajax.markup.html.form.AjaxButton;
import org.apache.wicket.markup.head.IHeaderResponse;
import org.apache.wicket.markup.head.JavaScriptReferenceHeaderItem;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.panel.Panel;
import org.apache.wicket.request.resource.JavaScriptResourceReference;
import org.cyclop.model.UserPreferences;
import org.cyclop.service.um.UserManager;
import org.cyclop.web.components.buttons.IconButton;
import org.cyclop.web.components.buttons.StateButton;

/** @author Maciej Miklas */
public class ButtonsPanel extends Panel {

	@Inject
	private UserManager userManager;

	private static final JavaScriptResourceReference JS_BUTTONS = new JavaScriptResourceReference(ButtonsPanel.class,
			"buttons.js");

	public ButtonsPanel(String id) {
		super(id);
		setRenderBodyOnly(true);
	}

	public ButtonsPanel withResultOrientation(final ButtonListener.ResultOrientationChange buttonListener) {
		UserPreferences preferences = userManager.readPreferences();
		int initialState = preferences.getResultOrientation();
		AjaxFallbackLink<Void> completion = new IconButton("resultOrientation", initialState,
				"glyphicon glyphicon-arrow-down", "glyphicon glyphicon-arrow-right") {
			@Override
			protected void onClick(AjaxRequestTarget target, int state) {
				UserPreferences preferences = userManager.readPreferences();
				preferences.setResultOrientation(state);
				userManager.storePreferences(preferences);
				buttonListener.onClick(target, state);
			}
		};
		add(completion);
		return this;
	}

	public ButtonsPanel withCompletion(final ButtonListener.CompletionChange buttonListener) {
		UserPreferences preferences = userManager.readPreferences();
		boolean completionEnabled = preferences.isShowCqlCompletionHint();
		AjaxFallbackLink<Void> completion = new StateButton("completion", completionEnabled, "btn btn-sm btn-info",
				"btn btn-sm btn-info active") {
			@Override
			protected void onClick(AjaxRequestTarget target, boolean pressed) {
				UserPreferences preferences = userManager.readPreferences();
				preferences.setShowCqlCompletionHint(pressed);
				userManager.storePreferences(preferences);

				buttonListener.onClick(target, pressed);
			}
		};
		add(completion);
		return this;
	}

	public ButtonsPanel withExportQueryResult(final ButtonListener.ExportQueryResult buttonListener) {
		AjaxFallbackLink<Void> exportQueryResult = new AjaxFallbackLink<Void>("exportQueryResult") {
			@Override
			public void onClick(AjaxRequestTarget target) {
				buttonListener.onClick(target);
			}
		};
		add(exportQueryResult);
		return this;
	}

	public ButtonsPanel withExecQuery(final ButtonListener.ExecQuery buttonListener, Form<String> form) {
		AjaxButton execQuery = new AjaxButton("execQuery", form) {
			@Override
			protected void onSubmit(AjaxRequestTarget target, Form<?> form) {
				buttonListener.onClick(target);
				target.appendJavaScript("queryExecutedResponse()");
			}
		};
		add(execQuery);
		return this;
	}

	public ButtonsPanel withAddToFavourites() {
		AjaxFallbackLink<Void> addToFavourites = new AjaxFallbackLink<Void>("addToFavourites") {
			@Override
			public void onClick(AjaxRequestTarget target) {
			}
		};
		add(addToFavourites);
		addToFavourites.setVisible(false);
		return this;
	}

	@Override
	public void renderHead(IHeaderResponse response) {
		super.renderHead(response);
		response.render(JavaScriptReferenceHeaderItem.forReference(JS_BUTTONS));
	}
}

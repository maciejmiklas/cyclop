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
package org.cyclop.web.components.infodialog;

import java.util.HashMap;
import java.util.Map;

import org.apache.wicket.AttributeModifier;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.panel.Panel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.util.template.PackageTextTemplate;
import org.cyclop.web.common.JsTextTemplate;

/** @author Maciej Miklas */
public class InfoDialog extends Panel {

	private final static JsTextTemplate INFO_JS = new JsTextTemplate(new PackageTextTemplate(InfoDialog.class,
			"infoDialog.js"));

	private final WebMarkupContainer infoDialog;

	private final StringModel titleModel;

	private final StringModel messageModel;

	public InfoDialog(String id) {
		super(id);
		infoDialog = new WebMarkupContainer("infoDialog");
		infoDialog.setVisible(false);
		infoDialog.setOutputMarkupPlaceholderTag(true);
		titleModel = new StringModel();
		infoDialog.add(new AttributeModifier("title", titleModel));

		add(infoDialog);

		messageModel = new StringModel();
		Label message = new Label("message", messageModel);
		infoDialog.add(message);
	}

	public void open(AjaxRequestTarget target, String linkNameToDisable, String title, String message) {
		titleModel.setObject(title);
		messageModel.setObject(message);
		infoDialog.setVisible(true);
		target.add(infoDialog);

		Map<String, String> jsVariables = new HashMap<>();
		jsVariables.put("infoDialogId", "#" + infoDialog.getMarkupId());
		jsVariables.put("linkNameToDisable", "#" + linkNameToDisable);
		String jsContent = INFO_JS.asString(jsVariables);
		target.appendJavaScript(jsContent);
	}

	private final static class StringModel implements IModel<String> {

		private String value = "";

		@Override
		public String getObject() {
			return value;
		}

		@Override
		public void setObject(String object) {
			this.value = object;
		}

		@Override
		public void detach() {
		}

	}
}

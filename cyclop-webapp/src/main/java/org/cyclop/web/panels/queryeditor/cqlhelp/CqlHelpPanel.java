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
package org.cyclop.web.panels.queryeditor.cqlhelp;

import java.io.IOException;
import java.net.URL;

import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.panel.Panel;
import org.apache.wicket.model.IModel;
import org.cyclop.model.ContextCqlCompletion;

import com.google.common.base.Charsets;
import com.google.common.io.Resources;

/** @author Maciej Miklas */
public class CqlHelpPanel extends Panel {

	private Label cqlHelpContent;

	private ContextCqlCompletion currentCompletion;

	public CqlHelpPanel(String id) {
		super(id);
		setOutputMarkupId(true);
		cqlHelpContent = new Label("helpContent", new CqlHelpContentModel());
		cqlHelpContent.setEscapeModelStrings(false);
		add(cqlHelpContent);
	}

	public void changeCompletion(ContextCqlCompletion currentCompletion) {
		this.currentCompletion = currentCompletion;
	}

	private class CqlHelpContentModel implements IModel<String> {

		@Override
		public String getObject() {
			if (currentCompletion == null) {
				return ";-)";
			}

			String name = "help_" + currentCompletion.queryName.name().toLowerCase() + ".html";
			try {

				URL url = Resources.getResource("/org/cyclop/web/panels/queryeditor/cqlhelp/help/" + name);

				String text = Resources.toString(url, Charsets.UTF_8);
				return text;
			} catch (IOException | IllegalArgumentException e) {
				return "Help file:'" + name + "' found :-(";
			}
		}

		@Override
		public void setObject(String object) {
		}

		@Override
		public void detach() {
		}
	}
}

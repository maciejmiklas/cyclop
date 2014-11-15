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
package org.cyclop.web.panels.queryeditor.completionhint;

import java.util.Iterator;

import org.apache.wicket.AttributeModifier;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.panel.Panel;
import org.apache.wicket.model.IModel;
import org.cyclop.model.ContextCqlCompletion;
import org.cyclop.model.CqlPart;

/** @author Maciej Miklas */
public class CompletionHintPanel extends Panel {

	private ContextCqlCompletion currentCompletion;

	private boolean lastCssSwitch;

	private final String headerText;

	public CompletionHintPanel(String id, String headerText) {
		super(id);
		this.headerText = headerText;
	}

	@Override
	protected void onInitialize() {
		super.onInitialize();

		setOutputMarkupPlaceholderTag(true);

		WebMarkupContainer cqlInfoHint = new WebMarkupContainer("infoHint");
		cqlInfoHint.add(new AttributeModifier("class", new CqlInfoHintCssModel()));
		add(cqlInfoHint);

		Label cqlInfoHintContent = new Label("hintContent", new CqlInfoHintModel());
		cqlInfoHintContent.setEscapeModelStrings(false);
		cqlInfoHint.add(cqlInfoHintContent);

		Label headerTextContent = new Label("headerText", headerText);
		cqlInfoHint.add(headerTextContent);
	}

	public void changeCompletion(ContextCqlCompletion newCompletion) {
		if (currentCompletion != null && newCompletion != null && currentCompletion.equals(newCompletion)) {
			return;
		}

		currentCompletion = newCompletion;
		lastCssSwitch = !lastCssSwitch;
	}

	private class CqlInfoHintCssModel implements IModel<String> {

		@Override
		public String getObject() {

			String css = lastCssSwitch ? "cq-queryHint-dialog alert alert-dismissable cq-queryHint-bodyA"
					: "cq-queryHint-dialog alert alert-dismissable cq-hint-bodyB";
			return css;
		}

		@Override
		public void setObject(String object) {
		}

		@Override
		public void detach() {
		}
	}

	private class CqlInfoHintModel implements IModel<String> {

		@Override
		public String getObject() {
			if (currentCompletion == null) {
				return ";-)";
			}

			StringBuilder buf = new StringBuilder();
			Iterator<? extends CqlPart> partIt = currentCompletion.cqlCompletion.minCompletion.iterator();
			while (partIt.hasNext()) {
				CqlPart part = partIt.next();

				String css;
				switch (part.type()) {
				case KEYWORD:
					css = "cq-queryEl-keyword";
					break;
				case TABLE:
					css = "cq-queryEl-table";
					break;
				case COLUMN:
					css = "cq-queryEl-column";
					break;
				case KEYSPACE:
					css = "cq-queryEl-kyespace";
					break;
				case INDEX:
					css = "cq-queryEl-index";
					break;
				case NOT_SUPPORTED:
					css = "cq-queryEl-notSupported";
					break;
				case KEYWORD_VALUE:
					css = "cq-queryEl-value";
					break;

				default:
					css = null;
					break;
				}

				if (css == null) {
					buf.append(part.toDisplayString());
				} else {
					buf.append("<span class=\"").append(css).append("\">").append(part.toDisplayString());
					if (partIt.hasNext()) {
						buf.append(", ");
					}
					buf.append("</span>");
				}
			}

			return buf.toString();
		}

		@Override
		public void setObject(String object) {
		}

		@Override
		public void detach() {
		}
	}
}

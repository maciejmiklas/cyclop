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
package org.cyclop.web.components.buttons;

import org.apache.commons.lang.Validate;
import org.apache.wicket.AttributeModifier;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.AjaxFallbackLink;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.model.IModel;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/** @author Maciej Miklas */
public abstract class IconButton extends AjaxFallbackLink<Void> {
	private final static Logger LOG = LoggerFactory.getLogger(IconButton.class);
	private final String[] cssStates;
	private int stateIdx;

	public IconButton(final String id, int initialState, String... cssStates) {
		super(id);

		Validate.notNull(cssStates, "cssStates");
		Validate.isTrue(cssStates.length > 0, "cssStates empty");
		this.cssStates = cssStates;

		if (initialState >= cssStates.length) {
			LOG.info("Reseting initial state: {} >= {}", initialState, cssStates.length);
			initialState = 0;
		}
		this.stateIdx = initialState;
	}

	@Override
	protected void onInitialize() {
		super.onInitialize();
		initIcon();
	}

	private void initIcon() {
		WebMarkupContainer icon = new WebMarkupContainer("icon");
		add(icon);
		icon.add(new AttributeModifier("class", new IModel<String>() {
			@Override
			public String getObject() {
				String css = cssStates[stateIdx];
				return css;
			}

			@Override
			public void setObject(String object) {

			}

			@Override
			public void detach() {
			}
		}));
	}

	@Override
	public final void onClick(AjaxRequestTarget target) {
		target.add(this);
		onClick(target, next());
	}

	private int next() {
		stateIdx++;
		if (stateIdx == cssStates.length) {
			stateIdx = 0;
		}
		return stateIdx;
	}

	protected abstract void onClick(AjaxRequestTarget target, int stateIdx);
}

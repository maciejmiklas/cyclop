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

import org.apache.wicket.AttributeModifier;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.AjaxFallbackLink;
import org.apache.wicket.model.IModel;

/** @author Maciej Miklas */
public abstract class StateButton extends AjaxFallbackLink<Void> {

	private boolean pressed;

	public StateButton(final String id, final boolean initialPressed, final String cssReleased, final String cssPressed) {
		super(id);
		this.pressed = initialPressed;

		add(new AttributeModifier("class", new IModel<String>() {
			@Override
			public String getObject() {
				String css = pressed ? cssPressed : cssReleased;
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
		pressed = !pressed;
		onClick(target, pressed);
	}

	protected abstract void onClick(AjaxRequestTarget target, boolean pressed);
}

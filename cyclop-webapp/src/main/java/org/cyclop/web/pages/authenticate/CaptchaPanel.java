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
package org.cyclop.web.pages.authenticate;

import org.apache.wicket.extensions.markup.html.captcha.CaptchaImageResource;
import org.apache.wicket.markup.html.form.RequiredTextField;
import org.apache.wicket.markup.html.image.NonCachingImage;
import org.apache.wicket.markup.html.panel.Panel;
import org.apache.wicket.model.CompoundPropertyModel;

/** @author Maciej Miklas */
public class CaptchaPanel extends Panel {

	private String captchaEntry;
	private CaptchaImageResource captchaImageResource;

	public CaptchaPanel(String id) {
		super(id);
		setDefaultModel(new CompoundPropertyModel<CaptchaPanel>(CaptchaPanel.this));
	}

	@Override
	protected void onInitialize() {
		super.onInitialize();

		captchaImageResource = new CaptchaImageResource();
		System.out.println("ChallengeId: " + captchaImageResource.getChallengeId());
		add(new NonCachingImage("captchaImage", captchaImageResource));
		add(new RequiredTextField<String>("captchaEntry"));
	}

	@Override
	protected void onConfigure() {
		super.onConfigure();
		captchaEntry = "";
	}

	public boolean verifyCaptcha() {
		return captchaImageResource.getChallengeId().equals(captchaEntry);
	}

}

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

import java.awt.image.BufferedImage;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.util.Properties;

import javax.imageio.ImageIO;

import org.apache.commons.lang3.RandomStringUtils;
import org.apache.wicket.markup.html.form.RequiredTextField;
import org.apache.wicket.markup.html.image.NonCachingImage;
import org.apache.wicket.markup.html.panel.Panel;
import org.apache.wicket.model.AbstractReadOnlyModel;
import org.apache.wicket.model.CompoundPropertyModel;
import org.apache.wicket.request.resource.DynamicImageResource;
import org.cyclop.common.AppConfig;
import org.cyclop.model.exception.ServiceException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.code.kaptcha.impl.DefaultKaptcha;
import com.google.code.kaptcha.util.Config;

/** @author Maciej Miklas */
public class CaptchaPanel extends Panel {
	private final static Logger LOG = LoggerFactory.getLogger(LoginPanel.class);

	private String captchaEntry;

	private CaptchaModel captchaModel;

	public CaptchaPanel(String id) {
		super(id);
		setDefaultModel(new CompoundPropertyModel<>(CaptchaPanel.this));
	}

	@Override
	protected void onInitialize() {
		super.onInitialize();
		captchaModel = new CaptchaModel();
		KaptchaImageResource captchaImageResource = new KaptchaImageResource(captchaModel);
		add(new NonCachingImage("captchaImage", captchaImageResource));
		add(new RequiredTextField<String>("captchaEntry"));
	}

	@Override
	protected void onConfigure() {
		super.onConfigure();
		captchaEntry = "";
		captchaModel.reset();
	}

	public boolean verifyCaptcha() {
		String modelVal = captchaModel.getObject();
		boolean result = modelVal.equals(captchaEntry);
		if (!result) {
			LOG.info("Captcha verification failed. {} != {}", modelVal, captchaEntry);
		}
		return result;
	}

	private static class CaptchaModel extends AbstractReadOnlyModel<String> {
		AppConfig config = AppConfig.get();
		String challenge;

		CaptchaModel() {
			reset();
		}

		public void reset() {
			challenge = RandomStringUtils.randomAlphabetic(config.login.captchaCharacters);
			LOG.debug("Generated captcha: {}", challenge);
		}

		@Override
		public String getObject() {
			return challenge;
		}

	}

	private static class KaptchaImageResource extends DynamicImageResource {

		private transient DefaultKaptcha producer;
		private final CaptchaModel model;

		public KaptchaImageResource(CaptchaModel model) {
			this.model = model;
			initKaptcha();
		}

		private void readObject(java.io.ObjectInputStream in) throws ClassNotFoundException, IOException {
			in.defaultReadObject();
			initKaptcha();
		}

		private void initKaptcha() {
			producer = new DefaultKaptcha();
			Config config = new Config(new Properties());
			producer.setConfig(config);
		}

		@Override
		protected byte[] getImageData(Attributes attributes) {
			BufferedImage img = producer.createImage(model.getObject());
			ByteArrayOutputStream baos = new ByteArrayOutputStream();
			try {
				ImageIO.write(img, "jpg", baos);
				baos.flush();
			} catch (IOException e) {
				throw new ServiceException("Error generating kaptcha image: " + e.getMessage(), e);
			}

			byte[] bytes = baos.toByteArray();
			return bytes;
		}

	}
}

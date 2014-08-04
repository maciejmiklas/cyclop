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
package org.cyclop.web.common;

import java.util.Map;

import org.apache.wicket.util.template.TextTemplate;
import org.apache.wicket.util.template.TextTemplateDecorator;

/** @author Maciej Miklas */
public class JsTextTemplate extends TextTemplateDecorator {

	public JsTextTemplate(TextTemplate textTemplate) {
		super(textTemplate);
	}

	@Override
	public String getBeforeTemplateContents() {
		return "";
	}

	@Override
	public String getAfterTemplateContents() {
		return "";
	}

	@Override
	public TextTemplate interpolate(Map<String, ?> variables) {
		return this;
	}

}

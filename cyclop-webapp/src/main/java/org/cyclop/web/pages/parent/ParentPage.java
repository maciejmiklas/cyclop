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
package org.cyclop.web.pages.parent;

import static org.cyclop.web.resources.ScriptsRef.BOOTSTRAP;
import static org.cyclop.web.resources.ScriptsRef.COMMON;
import static org.cyclop.web.resources.ScriptsRef.FLOAT_THEAD;
import static org.cyclop.web.resources.ScriptsRef.JQUERY_BROWSER;
import static org.cyclop.web.resources.ScriptsRef.JQUERY_TOOLS;
import static org.cyclop.web.resources.ScriptsRef.JQUERY_UI;

import org.apache.wicket.markup.head.IHeaderResponse;
import org.apache.wicket.markup.head.JavaScriptHeaderItem;
import org.apache.wicket.markup.html.WebPage;

/** @author Maciej Miklas */
public abstract class ParentPage extends WebPage {

	public ParentPage() {
		setVersioned(false);
	}

	@Override
	public void renderHead(IHeaderResponse response) {
		super.renderHead(response);
		response.render(JavaScriptHeaderItem.forReference(JQUERY_UI));
		response.render(JavaScriptHeaderItem.forReference(JQUERY_TOOLS));
		response.render(JavaScriptHeaderItem.forReference(JQUERY_BROWSER));
		response.render(JavaScriptHeaderItem.forReference(BOOTSTRAP));
		response.render(JavaScriptHeaderItem.forReference(COMMON));
		response.render(JavaScriptHeaderItem.forReference(FLOAT_THEAD));
	}
}

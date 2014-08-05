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
package org.cyclop.web.panels.about;

import org.apache.wicket.ajax.AbstractDefaultAjaxBehavior;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.panel.Panel;
import org.apache.wicket.model.IModel;
import org.cyclop.web.common.AjaxReloadSupport;

public class AboutPanel extends Panel implements AjaxReloadSupport {

    private AbstractDefaultAjaxBehavior browserCallback;

    public AboutPanel(String id) {
	super(id);
	WebMarkupContainer aboutContainer = initAboutContainer();
	initContent(aboutContainer);
	browserCallback = initBrowserCallback(aboutContainer);
    }

    private void initContent(final WebMarkupContainer aboutContainer) {
	aboutContainer.add(new Label("version", new IModel<String>() {

	    @Override
	    public void detach() {
	    }

	    @Override
	    public String getObject() {
		return "1.4.0";
	    }

	    @Override
	    public void setObject(String object) {
	    }
	}));
    }

    @Override
    public String getReloadCallbackUrl() {
	return browserCallback.getCallbackUrl().toString();
    }

    private WebMarkupContainer initAboutContainer() {
	WebMarkupContainer historyContainer = new WebMarkupContainer("aboutContainer");
	historyContainer.setOutputMarkupPlaceholderTag(true);
	historyContainer.setVisible(false);
	add(historyContainer);
	return historyContainer;
    }

    private AbstractDefaultAjaxBehavior initBrowserCallback(final WebMarkupContainer aboutContainer) {

	AbstractDefaultAjaxBehavior browserCallback = new AbstractDefaultAjaxBehavior() {

	    @Override
	    protected void respond(final AjaxRequestTarget target) {
		aboutContainer.setVisible(true);
		target.add(aboutContainer);
	    }
	};
	add(browserCallback);
	return browserCallback;
    }

    @Override
    public String getRemovableContentCssRef() {
	return ".cq-aboutContainer";
    }

}

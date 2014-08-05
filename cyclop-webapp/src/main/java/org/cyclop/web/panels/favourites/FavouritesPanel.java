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
package org.cyclop.web.panels.favourites;

import org.apache.wicket.ajax.AbstractDefaultAjaxBehavior;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.panel.Panel;
import org.apache.wicket.model.IModel;
import org.cyclop.model.QueryEntry;
import org.cyclop.web.common.AjaxReloadSupport;
import org.cyclop.web.common.ImmutableListModel;

/** @author Maciej Miklas */
public class FavouritesPanel extends Panel implements AjaxReloadSupport {

    private AbstractDefaultAjaxBehavior browserCallback;

    private int count = 0;

    public FavouritesPanel(String id) {
	super(id);
	WebMarkupContainer favouritesContainer = initFavouritesContainer();
	initFavouritesTable(favouritesContainer);
	browserCallback = initBrowserCallback(favouritesContainer);

	favouritesContainer.add(new Label("counter", new IModel<String>() {

	    @Override
	    public void detach() {
	    }

	    @Override
	    public String getObject() {
		return count + "";
	    }

	    @Override
	    public void setObject(String object) {
	    }
	}));
    }

    private ImmutableListModel<QueryEntry> initFavouritesTable(final WebMarkupContainer historyContainer) {
	ImmutableListModel<QueryEntry> model = new ImmutableListModel<>();
	return model;
    }

    @Override
    public String getReloadCallbackUrl() {
	return browserCallback.getCallbackUrl().toString();
    }

    private WebMarkupContainer initFavouritesContainer() {
	WebMarkupContainer historyContainer = new WebMarkupContainer("favouritesContainer");
	historyContainer.setOutputMarkupPlaceholderTag(true);
	historyContainer.setVisible(false);
	add(historyContainer);
	return historyContainer;
    }

    private AbstractDefaultAjaxBehavior initBrowserCallback(final WebMarkupContainer historyContainer) {

	AbstractDefaultAjaxBehavior browserCallback = new AbstractDefaultAjaxBehavior() {

	    @Override
	    protected void respond(final AjaxRequestTarget target) {
		count++;
		historyContainer.setVisible(true);
		target.add(historyContainer);
	    }
	};
	add(browserCallback);
	return browserCallback;
    }

    @Override
    public String getRemovableContentCssRef() {
	return ".cq-favouritesContainer";
    }

}

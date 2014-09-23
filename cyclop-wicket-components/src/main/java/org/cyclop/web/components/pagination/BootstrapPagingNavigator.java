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
package org.cyclop.web.components.pagination;

import com.google.common.collect.ImmutableList;
import org.apache.wicket.AttributeModifier;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.form.AjaxFormComponentUpdatingBehavior;
import org.apache.wicket.ajax.markup.html.navigation.paging.AjaxPagingNavigation;
import org.apache.wicket.ajax.markup.html.navigation.paging.AjaxPagingNavigationIncrementLink;
import org.apache.wicket.ajax.markup.html.navigation.paging.AjaxPagingNavigationLink;
import org.apache.wicket.ajax.markup.html.navigation.paging.AjaxPagingNavigator;
import org.apache.wicket.markup.ComponentTag;
import org.apache.wicket.markup.html.form.DropDownChoice;
import org.apache.wicket.markup.html.link.AbstractLink;
import org.apache.wicket.markup.html.link.ExternalLink;
import org.apache.wicket.markup.html.list.LoopItem;
import org.apache.wicket.markup.html.navigation.paging.IPageable;
import org.apache.wicket.markup.html.navigation.paging.IPageableItems;
import org.apache.wicket.markup.html.navigation.paging.IPagingLabelProvider;
import org.apache.wicket.markup.html.navigation.paging.PagingNavigation;
import org.apache.wicket.model.PropertyModel;

/** @author Maciej Miklas */
public class BootstrapPagingNavigator extends AjaxPagingNavigator {

	private final static ImmutableList<Long> CHOICES = ImmutableList.of(1L, 2L, 5L, 10L, 20L, 50L, 100L, 200L, 500L,
			1000L);

	public BootstrapPagingNavigator(String id, IPageableItems pageable, PagerConfigurator configurator) {
		super(id, pageable);
		initPageSizeChoice(pageable, configurator);
		if (configurator == null) {
			throw new IllegalArgumentException("Null coinfigurator");
		}
		pageable.setItemsPerPage(configurator.getInitialItemsPerPage());
	}

	public void reset() {
		getPageable().setCurrentPage(0);
	}

	public void setCurrentPage(long page) {
		getPageable().setCurrentPage(page);
	}

	public long getCurrentPage() {
		return getPageable().getCurrentPage();
	}

	// Link for: "1 | 2 | 3 | 4"
	@Override
	protected PagingNavigation newNavigation(String id, IPageable pageable, IPagingLabelProvider labelProvider) {
		return new AjaxPagingNavigation(id, pageable, labelProvider) {

			@Override
			protected LoopItem newItem(int iteration) {
				LoopItem item = super.newItem(iteration);

				// add css for enable/disable link
				long pageIndex = getStartIndex() + iteration;
				item.add(new AttributeModifier("class", new PagingNavigationCssModel(pageable, pageIndex, "active")));

				return item;
			}
		};
	}

	// Link for: first,last
	@Override
	protected AbstractLink newPagingNavigationLink(String id, IPageable pageable, int pageNumber) {
		ExternalLink navCont = new ExternalLink(id + "Cont", (String) null);

		// add css for enable/disable link
		long pageIndex = pageable.getCurrentPage() + pageNumber;
		navCont.add(new AttributeModifier("class", new PagingNavigationLinkCssModel(pageable, pageIndex, "disabled")));

		// change original wicket-link, so that it always generates href
		navCont.add(new AjaxPagingNavigationLink(id, pageable, pageNumber) {
			@Override
			protected void disableLink(ComponentTag tag) {
			}
		});
		return navCont;
	}

	// Link for: prev,next
	@Override
	protected AbstractLink newPagingNavigationIncrementLink(String id, IPageable pageable, int increment) {
		ExternalLink navCont = new ExternalLink(id + "Cont", (String) null);

		// add css for enable/disable link
		long pageIndex = pageable.getCurrentPage() + increment;
		navCont.add(new AttributeModifier("class", new NavigationIncrementLinkCssModel(pageable, pageIndex)));

		// change original wicket-link, so that it always generates href
		navCont.add(new AjaxPagingNavigationIncrementLink(id, pageable, increment) {
			@Override
			protected void disableLink(ComponentTag tag) {
			}
		});
		return navCont;
	}

	private void initPageSizeChoice(final IPageableItems pageable, final PagerConfigurator configurator) {
		final DropDownChoice<Long> choice = new DropDownChoice<>("pageSize", CHOICES);
		choice.setDefaultModel(new PropertyModel<Long>(pageable, "itemsPerPage"));
		add(choice);

		choice.add(new AjaxFormComponentUpdatingBehavior("onchange") {

			@Override
			protected void onUpdate(AjaxRequestTarget target) {
				reset();
				onAjaxEvent(target);
				configurator.onItemsPerPageChanged(target, pageable.getItemsPerPage());
			}
		});
	}
}

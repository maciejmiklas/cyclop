package org.cyclop.web.components.pagination;

import org.apache.wicket.ajax.markup.html.navigation.paging.AjaxPagingNavigator;
import org.apache.wicket.markup.html.link.AbstractLink;
import org.apache.wicket.markup.html.navigation.paging.IPageable;
import org.apache.wicket.markup.html.navigation.paging.IPagingLabelProvider;

/** @author Maciej Miklas */
public class IterablePagingNavigator extends AjaxPagingNavigator {

	public IterablePagingNavigator(String id, IPageable pageable) {
		super(id, pageable);
	}

	public IterablePagingNavigator(String id, IPageable pageable, IPagingLabelProvider labelProvider) {
		super(id, pageable, labelProvider);
	}

	@Override
	protected AbstractLink newPagingNavigationLink(String id, IPageable pageable, int pageNumber) {
		return new PageJumpNavigationLink(id, pageable, pageNumber);
	}
}

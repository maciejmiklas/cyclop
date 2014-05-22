package org.cyclop.web.components.pagination;

import org.apache.wicket.ajax.markup.html.navigation.paging.AjaxPagingNavigationLink;
import org.apache.wicket.markup.html.navigation.paging.IPageable;
import org.cyclop.web.components.iterablegrid.IterableGridView;

/**
 * Link for: first,last page
 *
 * @author Maciej Miklas
 */
public class PageJumpNavigationLink extends AjaxPagingNavigationLink {

	public PageJumpNavigationLink(String id, IPageable pageable, long pageNumber) {
		super(id, pageable, pageNumber);
	}

	@Override
	protected boolean isLinkEnabled() {
		if (pageable instanceof IterableGridView) {
			IterableGridView iterableGridView = (IterableGridView) pageable;
			if (iterableGridView.hasMoreData()) {
				return false;
			}
		}

		return super.isLinkEnabled();
	}
}

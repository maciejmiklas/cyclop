package org.cyclop.web.components.pagination;

import org.apache.wicket.markup.html.navigation.paging.IPageable;
import org.cyclop.web.components.iterablegrid.IterableGridView;

/** @author Maciej Miklas */
public class PagingNavigationLinkCssModel extends PagingNavigationCssModel {

	public PagingNavigationLinkCssModel(IPageable pageable, long pageNumber, String css) {
		super(pageable, pageNumber, css);
	}

	@Override
	public boolean isEnabled() {

		// goto-last-page for IterableGridView can be only enabled if all elements has been read from iterator
		if (pageNumber == -1 && pageable instanceof IterableGridView) {
			IterableGridView iterableGridView = (IterableGridView) pageable;
			if (iterableGridView.hasMoreData()) {
				return false;
			}
		}
		return super.isEnabled();
	}
}

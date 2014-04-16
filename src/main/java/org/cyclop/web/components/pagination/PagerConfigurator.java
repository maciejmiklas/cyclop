package org.cyclop.web.components.pagination;

import org.apache.wicket.ajax.AjaxRequestTarget;

import java.io.Serializable;

public interface PagerConfigurator extends Serializable {

	long getInitialItemsPerPage();

	void onItemsPerPageChanged(AjaxRequestTarget target, long newItemsPerPage);

}

package org.cyclop.web.pages.parent;

import org.apache.wicket.markup.head.IHeaderResponse;
import org.apache.wicket.markup.head.JavaScriptHeaderItem;
import org.apache.wicket.markup.html.WebPage;

import static org.cyclop.web.common.ScriptsRef.BOOTSTRAP;
import static org.cyclop.web.common.ScriptsRef.COMMON;
import static org.cyclop.web.common.ScriptsRef.JQUERY_TOOLS;
import static org.cyclop.web.common.ScriptsRef.JQUERY_UI;

/** @author Maciej Miklas */
public class ParentPage extends WebPage {

	@Override
	public void renderHead(IHeaderResponse response) {
		super.renderHead(response);
		response.render(JavaScriptHeaderItem.forReference(BOOTSTRAP));
		response.render(JavaScriptHeaderItem.forReference(JQUERY_UI));
		response.render(JavaScriptHeaderItem.forReference(JQUERY_TOOLS));
		response.render(JavaScriptHeaderItem.forReference(COMMON));
	}
}

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

			// TODO read version from manifest file
			@Override
			public String getObject() {
				return "1.4";
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

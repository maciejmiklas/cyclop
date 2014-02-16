package org.cyclop.web.panels.history;

import org.apache.wicket.ajax.AbstractDefaultAjaxBehavior;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.panel.Panel;
import org.apache.wicket.model.IModel;

/** @author Maciej Miklas */
public class HistoryPanel extends Panel {

	private String browserCallbackUrl;

	int count;

	public HistoryPanel(String id) {
		super(id);

		Label counter = new Label("counter", new IModel<String>() {
			@Override
			public String getObject() {
				count++;
				return count + "";
			}

			@Override
			public void setObject(String o) {
			}

			@Override
			public void detach() {
			}
		});
		add(counter);


	}

	public void init() {
		AbstractDefaultAjaxBehavior browserCallback = new AbstractDefaultAjaxBehavior() {
			protected void respond(final AjaxRequestTarget target) {
				System.out.println("AAAAAAAAAAAA - browserCallback");
			}
		};
		add(browserCallback);
		browserCallbackUrl = browserCallback.getCallbackUrl().toString();
		System.out.println(browserCallback.getCallbackScript());
	}

	public String getBrowserCallbackUrl() {
		return browserCallbackUrl;
	}
}

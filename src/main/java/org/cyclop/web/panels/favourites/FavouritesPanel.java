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

public class FavouritesPanel extends Panel implements AjaxReloadSupport {

	private AbstractDefaultAjaxBehavior browserCallback;

	private int count = 0;

	public FavouritesPanel(String id) {
		super(id);
		WebMarkupContainer favouritesContainer = initFavouritesContainer();
		ImmutableListModel<QueryEntry> model = initFavouritesTable(favouritesContainer);
		browserCallback = initBrowserCallback(model, favouritesContainer);

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

	private AbstractDefaultAjaxBehavior initBrowserCallback(final ImmutableListModel<QueryEntry> model,
															final WebMarkupContainer historyContainer) {

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

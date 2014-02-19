package org.cyclop.web.panels.history;

import org.apache.wicket.Component;
import org.apache.wicket.ajax.AbstractDefaultAjaxBehavior;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.list.ListItem;
import org.apache.wicket.markup.html.list.PageableListView;
import org.apache.wicket.markup.html.panel.Panel;
import org.apache.wicket.model.IModel;
import org.cyclop.common.AppConfig;
import org.cyclop.model.QueryEntry;
import org.cyclop.service.queryprotocoling.HistoryService;

import javax.inject.Inject;
import java.util.List;

/** @author Maciej Miklas */
public class HistoryPanel extends Panel {

	private AbstractDefaultAjaxBehavior browserCallback;

	@Inject
	private HistoryService historyService;

	public HistoryPanel(String id) {
		super(id);

		PageableListView<QueryEntry> grid = initHistoryTable();
		WebMarkupContainer gridContainer = new WebMarkupContainer("historyGridContainer");
		gridContainer.add(grid);
		gridContainer.setOutputMarkupId(true);
		add(gridContainer);

		browserCallback = initBrowserCallback(gridContainer);
		add(browserCallback);//TODO remove add() calls from init methods in whole project - like here
	}

	private AbstractDefaultAjaxBehavior initBrowserCallback(final Component comp) {
		AbstractDefaultAjaxBehavior browserCallback = new AbstractDefaultAjaxBehavior() {
			protected void respond(final AjaxRequestTarget target) {
				target.add(comp);
			}
		};
		return browserCallback;
	}

	private PageableListView<QueryEntry> initHistoryTable() {
		PageableListView<QueryEntry> grid = new PageableListView<QueryEntry>("historyGrid", new QueryHistoryModel(),
				AppConfig.get().history.queriesPerPage) {

			@Override
			protected void populateItem(ListItem<QueryEntry> item) {
				QueryEntry entry = item.getModel().getObject();

				Label executedOn = new Label("executedOn", entry.executedOnUtc);
				item.add(executedOn);

				Label query = new Label("query", entry.query.part);
				item.add(query);
			}

		};
		grid.setOutputMarkupId(true);
		return grid;
	}

	public String getRefreshContentCallbackUrl() {
		return browserCallback.getCallbackUrl().toString();
	}

	private class QueryHistoryModel implements IModel<List<QueryEntry>> {
		@Override
		public void detach() {
		}

		@Override
		public List<QueryEntry> getObject() {
			return historyService.read().asList();
		}

		@Override
		public void setObject(List<QueryEntry> history) {
		}

	}
}

package org.cyclop.web.panels.history;

import com.google.common.collect.ImmutableList;
import org.apache.wicket.ajax.AbstractDefaultAjaxBehavior;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.link.BookmarkablePageLink;
import org.apache.wicket.markup.html.list.ListItem;
import org.apache.wicket.markup.html.list.PageableListView;
import org.apache.wicket.markup.html.panel.Panel;
import org.apache.wicket.request.mapper.parameter.PageParameters;
import org.cyclop.common.AppConfig;
import org.cyclop.model.CqlQuery;
import org.cyclop.model.CqlQueryName;
import org.cyclop.model.QueryEntry;
import org.cyclop.model.QueryHistory;
import org.cyclop.service.converter.DataConverter;
import org.cyclop.service.queryprotocoling.HistoryService;
import org.cyclop.web.common.AjaxReloadSupport;
import org.cyclop.web.common.ImmutableListModel;
import org.cyclop.web.components.pagination.BootstrapPagingNavigator;
import org.cyclop.web.pages.main.MainPage;

import javax.inject.Inject;
import java.util.UUID;

/** @author Maciej Miklas */
public class HistoryPanel extends Panel implements AjaxReloadSupport {

	private AbstractDefaultAjaxBehavior browserCallback;

	private BootstrapPagingNavigator pager;

	@Inject
	private HistoryService historyService;

	@Inject
	private DataConverter converter;

	private PageableListView<QueryEntry> historyTable;

	public HistoryPanel(String id) {
		super(id);
		WebMarkupContainer historyContainer = initHistoryContainer();
		ImmutableListModel<QueryEntry> model = initHistoryTable(historyContainer);
		browserCallback = initBrowserCallback(model, historyContainer);
	}

	private WebMarkupContainer initHistoryContainer() {
		WebMarkupContainer historyContainer = new WebMarkupContainer("historyContainer");
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
				ImmutableList<QueryEntry> historyList = historyService.read().copyAsList();
				model.setObject(historyList);

				resetHistoryTable();

				historyContainer.setVisible(true);
				target.add(historyContainer);
			}
		};
		add(browserCallback);
		return browserCallback;
	}

	private void resetHistoryTable() {
		historyTable.removeAll();
		pager.getPageable().setCurrentPage(0);
	}

	// TODO !!
	static int a = 10;

	// TODO !!
	private QueryHistory createHist() {
		a += 10;
		QueryHistory qh = new QueryHistory();
		for (int i = 0; i < 100; i++) {
			qh.add(new QueryEntry(new CqlQuery(CqlQueryName.SELECT,
					a + "select myfil, " + UUID.randomUUID() + ", bsefef, afsf f,e from cqldemo.books where id=" + i),
					i, i * 3));
		}
		return qh;
	}

	private ImmutableListModel<QueryEntry> initHistoryTable(final WebMarkupContainer historyContainer) {
		ImmutableListModel<QueryEntry> model = new ImmutableListModel<>();

		historyTable = new PageableListView<QueryEntry>("historyRow", model, AppConfig.get().history.queriesPerPage) {

			@Override
			protected void populateItem(ListItem<QueryEntry> item) {
				QueryEntry entry = item.getModel().getObject();

				populateExecutedOn(item, entry);
				populateRuntime(item, entry);
				populateQuery(item, entry);
			}
		};
		historyContainer.add(historyTable);
		pager = new BootstrapPagingNavigator("historyPager", historyTable);
		historyContainer.add(pager);

		return model;
	}

	private void populateQuery(ListItem<QueryEntry> item, QueryEntry entry) {
		PageParameters queryLinkParams = new PageParameters();
		queryLinkParams.add("cql", entry.query.part);
		BookmarkablePageLink<Void> link = new BookmarkablePageLink<>("queryLink", MainPage.class, queryLinkParams);
		item.add(link);

		item.add(new Label("query", entry.query.part));
	}

	private void populateExecutedOn(ListItem<QueryEntry> item, QueryEntry entry) {
		String dateStr = converter.convert(entry.executedOnUtc);
		Label executedOn = new Label("executedOn", dateStr);
		item.add(executedOn);
	}

	private void populateRuntime(ListItem<QueryEntry> item, QueryEntry entry) {
		String dateStr = Long.toString(entry.runTime);
		Label executedOn = new Label("runtime", dateStr);
		item.add(executedOn);
	}

	@Override
	public String getReloadCallbackUrl() {
		return browserCallback.getCallbackUrl().toString();
	}

	@Override
	public String getReloadableContentCssRef() {
		return ".cq-historyContainer";
	}

}

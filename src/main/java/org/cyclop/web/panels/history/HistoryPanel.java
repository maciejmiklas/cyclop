package org.cyclop.web.panels.history;

import javax.inject.Inject;

import org.apache.commons.lang.StringUtils;
import org.apache.wicket.ajax.AbstractDefaultAjaxBehavior;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.form.OnChangeAjaxBehavior;
import org.apache.wicket.ajax.markup.html.AjaxFallbackLink;
import org.apache.wicket.markup.head.IHeaderResponse;
import org.apache.wicket.markup.head.JavaScriptReferenceHeaderItem;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.form.TextField;
import org.apache.wicket.markup.html.link.BookmarkablePageLink;
import org.apache.wicket.markup.html.list.ListItem;
import org.apache.wicket.markup.html.list.PageableListView;
import org.apache.wicket.markup.html.panel.Panel;
import org.apache.wicket.request.mapper.parameter.PageParameters;
import org.apache.wicket.request.resource.JavaScriptResourceReference;
import org.apache.wicket.util.string.Strings;
import org.cyclop.common.AppConfig;
import org.cyclop.common.StringHelper;
import org.cyclop.common.StringHelper.StringDecorator;
import org.cyclop.model.FilterResult;
import org.cyclop.model.QueryEntry;
import org.cyclop.service.converter.DataConverter;
import org.cyclop.service.queryprotocoling.HistoryService;
import org.cyclop.service.search.FilterFieldAccessor;
import org.cyclop.service.search.SearchService;
import org.cyclop.web.common.AjaxReloadSupport;
import org.cyclop.web.common.ImmutableListModel;
import org.cyclop.web.common.TextModel;
import org.cyclop.web.components.pagination.BootstrapPagingNavigator;
import org.cyclop.web.pages.main.MainPage;

import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableSet;

/** @author Maciej Miklas */
public class HistoryPanel extends Panel implements AjaxReloadSupport {

    private static final JavaScriptResourceReference JS_HISTORY = new JavaScriptResourceReference(
	    HistoryPanel.class,
	    "history.js");

    private final AbstractDefaultAjaxBehavior browserCallback;

    // TODO replace with listener
    private BootstrapPagingNavigator pager;

    @Inject
    private HistoryService historyService;

    @Inject
    private DataConverter converter;

    @Inject
    private SearchService<QueryEntry> searchService;

    private PageableListView<QueryEntry> historyTable;

    private ImmutableSet<String> filterKeywords;

    private final static QueryEntryFilterFieldAccessor FILTER_ACCESSOR = new QueryEntryFilterFieldAccessor();

    private final static KeywordDecorator KEYWORD_DECORATOR = new KeywordDecorator();

    private final ImmutableListModel<QueryEntry> historyModel;

    public HistoryPanel(String id) {
	super(id);
	WebMarkupContainer historyContainer = initHistoryContainer();
	historyModel = initHistoryTable(historyContainer);
	browserCallback = initBrowserCallback(historyContainer);
	initFilter(historyContainer);
    }

    private String decorateQueryWithFilter(QueryEntry entry) {
	String query = Strings.escapeMarkup(entry.query.part).toString();
	if (filterKeywords != null) {
	    query = StringHelper.decorate(
		    query,
		    KEYWORD_DECORATOR,
		    filterKeywords.toArray(new String[filterKeywords.size()]));
	}
	return query;
    }

    @Override
    public String getReloadCallbackUrl() {
	return browserCallback.getCallbackUrl().toString();
    }

    @Override
    public String getRemovableContentCssRef() {
	return ".cq-historyContainer";
    }

    private AbstractDefaultAjaxBehavior initBrowserCallback(final WebMarkupContainer historyContainer) {

	AbstractDefaultAjaxBehavior browserCallback = new AbstractDefaultAjaxBehavior() {

	    @Override
	    protected void respond(final AjaxRequestTarget target) {
		ImmutableList<QueryEntry> historyList = historyService.read().copyAsList();
		rebuildHistoryTable(historyList, null);

		historyContainer.setVisible(true);
		target.add(historyContainer);
	    }
	};
	add(browserCallback);
	return browserCallback;
    }

    private void initFilter(final WebMarkupContainer historyContainer) {
	final TextModel filterFieldModel = new TextModel();
	final TextField<String> filterField = new TextField<String>("filterField", filterFieldModel);
	filterField.setOutputMarkupId(true);

	filterField.add(new FilterBehaviour(filterFieldModel, historyContainer));
	add(filterField);

	AjaxFallbackLink<Void> resetFilter = new AjaxFallbackLink<Void>("resetFilter") {
	    @Override
	    public void onClick(AjaxRequestTarget target) {
		ImmutableList<QueryEntry> historyToUpdate = historyService.read().copyAsList();

		rebuildHistoryTable(historyToUpdate, null);
		filterFieldModel.setObject("");
		target.add(filterField);
		target.add(historyContainer);
		target.appendJavaScript("initResetButton();");
	    }
	};
	add(resetFilter);
    }

    private WebMarkupContainer initHistoryContainer() {
	WebMarkupContainer historyContainer = new WebMarkupContainer("historyContainer");
	historyContainer.setOutputMarkupPlaceholderTag(true);
	historyContainer.setVisible(false);
	add(historyContainer);
	return historyContainer;
    }

    private ImmutableListModel<QueryEntry> initHistoryTable(final WebMarkupContainer historyContainer) {
	ImmutableListModel<QueryEntry> model = new ImmutableListModel<>();

	historyTable = new PageableListView<QueryEntry>(
		"historyRow",
		model,
		AppConfig.get().history.queriesPerPage) {

	    @Override
	    protected void populateItem(ListItem<QueryEntry> item) {
		QueryEntry entry = item.getModel().getObject();

		populateExecutedOn(item, entry);
		populateRuntime(item, entry);
		populateResultsSize(item, entry);
		populateQuery(item, entry);
	    }
	};
	historyContainer.add(historyTable);
	pager = new BootstrapPagingNavigator("historyPager", historyTable);
	historyContainer.add(pager);

	return model;
    }

    private void populateExecutedOn(ListItem<QueryEntry> item, QueryEntry entry) {
	String dateStr = converter.convert(entry.executedOnUtc);
	Label executedOn = new Label("executedOn", dateStr);
	item.add(executedOn);
    }

    private void populateQuery(ListItem<QueryEntry> item, QueryEntry entry) {
	PageParameters queryLinkParams = new PageParameters();
	queryLinkParams.add("cql", entry.query.part);
	BookmarkablePageLink<Void> link = new BookmarkablePageLink<>(
		"queryLink",
		MainPage.class,
		queryLinkParams);
	item.add(link);

	Label queryLabel = new Label("query", decorateQueryWithFilter(entry));
	queryLabel.setEscapeModelStrings(false);
	item.add(queryLabel);
    }

    private void populateResultsSize(ListItem<QueryEntry> item, QueryEntry entry) {
	String dateStr = Long.toString(entry.resultsSize);
	Label executedOn = new Label("resultsSize", dateStr);
	item.add(executedOn);
    }

    private void populateRuntime(ListItem<QueryEntry> item, QueryEntry entry) {
	String dateStr = Long.toString(entry.runTime);
	Label executedOn = new Label("runtime", dateStr);
	item.add(executedOn);
    }

    private void rebuildHistoryTable(
	    ImmutableList<QueryEntry> historyList,
	    ImmutableSet<String> filterKeywords) {
	historyModel.setObject(historyList);
	historyTable.removeAll();
	pager.getPageable().setCurrentPage(0);
	this.filterKeywords = filterKeywords;
    }

    @Override
    public void renderHead(IHeaderResponse response) {
	super.renderHead(response);
	response.render(JavaScriptReferenceHeaderItem.forReference(JS_HISTORY));
    }

    private final class FilterBehaviour extends OnChangeAjaxBehavior {

	private final TextModel filterFieldModel;

	private final WebMarkupContainer historyContainer;

	public FilterBehaviour(TextModel filterFieldModel, WebMarkupContainer historyContainer) {
	    this.filterFieldModel = filterFieldModel;
	    this.historyContainer = historyContainer;
	}

	@Override
	protected void onUpdate(AjaxRequestTarget target) {
	    ImmutableList<QueryEntry> historyToUpdate = historyService.read().copyAsList();
	    ImmutableSet<String> kwds = null;

	    String filterValue = filterFieldModel.getObject();
	    filterValue = StringUtils.trimToNull(filterValue);
	    if (filterValue != null) {
		String[] kwordsArr = filterValue.split(" ");
		FilterResult<QueryEntry> filterResult = searchService.filter(
			historyToUpdate,
			FILTER_ACCESSOR,
			kwordsArr);
		if (filterResult != null) {
		    historyToUpdate = filterResult.result;
		    kwds = filterResult.normalizedKeywords;
		}
	    }

	    rebuildHistoryTable(historyToUpdate, kwds);
	    target.add(historyContainer);
	}

    }

    private final static class KeywordDecorator implements StringDecorator {
	@Override
	public String decorate(String in) {
	    return prefix() + in + postfix();
	}

	@Override
	public String postfix() {
	    return "</span>";
	}

	@Override
	public String prefix() {
	    return "<span class='cq-filterKeyword'>";
	}
    }

    private final static class QueryEntryFilterFieldAccessor implements FilterFieldAccessor<QueryEntry> {
	@Override
	public String getText(QueryEntry obj) {
	    return obj.query.partLc;
	}
    }

}

package org.cyclop.web.pages.cqlcommander;

import org.apache.wicket.Component;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.authroles.authorization.strategies.role.Roles;
import org.apache.wicket.authroles.authorization.strategies.role.annotations.AuthorizeInstantiation;
import org.apache.wicket.injection.Injector;
import org.apache.wicket.markup.html.panel.Panel;
import org.apache.wicket.request.mapper.parameter.PageParameters;
import org.apache.wicket.util.string.StringValue;
import org.cyclop.model.ContextCqlCompletion;
import org.cyclop.model.CqlQuery;
import org.cyclop.model.CqlSelectResult;
import org.cyclop.model.UserPreferences;
import org.cyclop.service.converter.CsvQueryResultExporter;
import org.cyclop.service.um.UserManager;
import org.cyclop.web.pages.cqlcommander.buttons.ButtonListener;
import org.cyclop.web.pages.cqlcommander.buttons.ButtonsPanel;
import org.cyclop.web.pages.cqlcommander.completionhint.CompletionHintPanel;
import org.cyclop.web.pages.cqlcommander.cqlhelp.CqlHelpPanel;
import org.cyclop.web.pages.cqlcommander.editor.CompletionChangeListener;
import org.cyclop.web.pages.cqlcommander.editor.QueryEditorPanel;
import org.cyclop.web.pages.cqlcommander.export.QueryResultExport;
import org.cyclop.web.pages.cqlcommander.verticalresult.QueryResultVerticalPanel;
import org.cyclop.web.pages.parent.ParentPage;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.inject.Inject;

/** @author Maciej Miklas */
@AuthorizeInstantiation(Roles.ADMIN)
public class CqlCommanderPanel extends Panel {

	private final CqlHelpPanel cqlHelpPanel;

	private final CompletionHintPanel cqlCompletionHintPanel;

	private boolean queryRunning = false;

	private final static Logger LOG = LoggerFactory.getLogger(CqlCommanderPanel.class);

	private CqlSelectResult lastQueryResult;

	private CqlQuery lastQuery;

	private QueryResultExport queryResultExport;

	@Inject
	private CsvQueryResultExporter exporter;

	@Inject
	private UserManager userManager;

	public CqlCommanderPanel(String id, PageParameters params) {
		super(id);
		Injector.get().inject(this);
		setRenderBodyOnly(true);

		cqlHelpPanel = new CqlHelpPanel("cqlHelp");
		add(cqlHelpPanel);

		cqlCompletionHintPanel = new CompletionHintPanel("cqlInfoHint", "Completion Hint");
		add(cqlCompletionHintPanel);

		QueryResultVerticalPanel queryResultVerticalPanel = initQueryResultPanel();
		QueryEditorPanel queryEditorPanel = initQueryEditorPanel(params);

		UserPreferences preferences = userManager.readPreferences();
		boolean completionEnabled = preferences.getShowCqlCompletionHint();
		cqlCompletionHintPanel.setVisible(completionEnabled);
		initButtons(queryEditorPanel, queryResultVerticalPanel, completionEnabled);

		queryResultExport = new QueryResultExport(this, exporter);
	}

	private QueryResultVerticalPanel initQueryResultPanel() {
		QueryResultVerticalPanel queryResultVerticalPanel = new QueryResultVerticalPanel("queryResultVerticalPanel");
		add(queryResultVerticalPanel);
		return queryResultVerticalPanel;
	}

	private QueryEditorPanel initQueryEditorPanel(PageParameters params) {

		StringValue editorContentVal = params.get("cql");
		String editorContent = editorContentVal == null ? null : editorContentVal.toString();

		QueryEditorPanel queryEditorPanel = new QueryEditorPanel("queryEditorPanel", editorContent);
		add(queryEditorPanel);
		queryEditorPanel.setOutputMarkupPlaceholderTag(true);
		queryEditorPanel.setOutputMarkupId(true);

		queryEditorPanel.registerCompletionChangeListener(new CompletionChangeHelp());
		queryEditorPanel.registerCompletionChangeListener(new CompletionChangeHint());
		return queryEditorPanel;
	}

	private ButtonsPanel initButtons(final QueryEditorPanel queryEditorPanel,
									 final QueryResultVerticalPanel queryResultVerticalPanel,
									 boolean completionEnabled) {
		ButtonListener buttonListener = new ButtonListener() {

			@Override
			public void onClickQueryResultExport(AjaxRequestTarget target) {
				queryResultExport.initiateDownload(target, lastQuery, lastQueryResult);
			}

			@Override
			public void onClickExecCql(AjaxRequestTarget target) {

				// this cannot happen, because java script disables execute button - it's DOS prevention
				if (queryRunning) {
					LOG.warn("Query still running - cannot execute second one");
					return;
				}

				queryResultVerticalPanel.setVisible(true);
				target.add(queryResultVerticalPanel);
				CqlQuery query = queryEditorPanel.getEditorContent();
				if (query == null) {
					return;
				}
				queryRunning = true;
				try {
					CqlSelectResult queryResult = queryResultVerticalPanel.executeQuery(query, target);
					if (queryResult != null) {
						lastQueryResult = queryResult;
						lastQuery = query;
					}
				} finally {
					queryRunning = false;
				}
			}

			@Override
			public void onClickCompletion(AjaxRequestTarget target, boolean pressed) {
				cqlCompletionHintPanel.setVisible(pressed);
				target.add(cqlCompletionHintPanel);

				UserPreferences preferences = userManager.readPreferences();
				preferences.setShowCqlCompletionHint(pressed);
				userManager.storePreferences(preferences);
			}

			@Override
			public void onClickLogOut() {
				getSession().invalidate();
			}
		};

		ButtonsPanel buttonsPanel = new ButtonsPanel("buttons", buttonListener, completionEnabled);
		add(buttonsPanel);
		return buttonsPanel;
	}

	private final class CompletionChangeHelp implements CompletionChangeListener {

		@Override
		public void onCompletionChange(ContextCqlCompletion currentCompletion) {
			cqlHelpPanel.changeCompletion(currentCompletion);
		}

		@Override
		public Component getReferencesForRefresh() {
			return cqlHelpPanel;
		}
	}

	private final class CompletionChangeHint implements CompletionChangeListener {

		@Override
		public void onCompletionChange(ContextCqlCompletion currentCompletion) {
			cqlCompletionHintPanel.changeCompletion(currentCompletion);
		}

		@Override
		public Component getReferencesForRefresh() {
			return cqlCompletionHintPanel;
		}
	}

}

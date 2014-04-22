package org.cyclop.web.panels.queryimport;

import com.google.common.collect.ImmutableList;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.form.AjaxButton;
import org.apache.wicket.markup.head.IHeaderResponse;
import org.apache.wicket.markup.head.JavaScriptReferenceHeaderItem;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.form.CheckBox;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.form.upload.FileUpload;
import org.apache.wicket.markup.html.form.upload.FileUploadField;
import org.apache.wicket.markup.html.list.ListItem;
import org.apache.wicket.markup.html.list.PageableListView;
import org.apache.wicket.markup.html.panel.Panel;
import org.apache.wicket.model.CompoundPropertyModel;
import org.apache.wicket.protocol.http.WebSession;
import org.apache.wicket.request.resource.JavaScriptResourceReference;
import org.apache.wicket.util.lang.Bytes;
import org.cyclop.common.AppConfig;
import org.cyclop.model.UserPreferences;
import org.cyclop.service.importer.QueryImporter;
import org.cyclop.service.importer.model.ImportConfig;
import org.cyclop.service.importer.model.ImportStats;
import org.cyclop.service.um.UserManager;
import org.cyclop.web.common.ImmutableListModel;
import org.cyclop.web.common.ImmutableListModel.ModelChangeListener;
import org.cyclop.web.common.JsFunctionBuilder;
import org.cyclop.web.components.pagination.BootstrapPagingNavigator;
import org.cyclop.web.components.pagination.PagerConfigurator;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.inject.Inject;
import javax.inject.Named;
import java.io.ByteArrayInputStream;
import java.text.DecimalFormat;
import java.text.NumberFormat;

/** @author Maciej Miklas */
public class QueryImportPanel extends Panel {

	private final static Logger LOG = LoggerFactory.getLogger(QueryImportPanel.class);

	private static final JavaScriptResourceReference JS_IMPORT = new JavaScriptResourceReference(QueryImportPanel.class,
			"queryImport.js");

	private final ImmutableListModel<ImportResult> resultModel;

	private final AppConfig conf = AppConfig.get();

	private final WebMarkupContainer importResultContainer;

	@Inject
	@Named(QueryImporter.IMPL_SYNC)
	private QueryImporter importer;

	@Inject
	private UserManager um;

	private ThreadLocal<DecimalFormat> NUMBER_FORMAT = new ThreadLocal<DecimalFormat>() {
		protected DecimalFormat initialValue() {
			DecimalFormat nf = (DecimalFormat) NumberFormat.getNumberInstance(WebSession.get().getLocale());
			nf.applyPattern("###.###");
			return nf;
		}

		;
	};

	public QueryImportPanel(String id) {
		super(id);
		ImportOptions importOptions = createImportOptions();
		setDefaultModel(new CompoundPropertyModel<ImportOptions>(importOptions));

		Form<?> form = initFileUpload(importOptions);
		initUploadOptions(form);

		importResultContainer = initImportResultContainer();
		resultModel = initResultTable(importResultContainer);
	}

	private ImportOptions createImportOptions() {
		ImportOptions importOptions = new ImportOptions();
		UserPreferences prefs = um.readPreferences();
		importOptions.setContinueWithErrors(prefs.isImportContinueWithErrors());
		importOptions.setIncludeInHistory(prefs.isImportIncludeInHistory());
		return importOptions;
	}

	private String createStatsMessage(ImportStats stats) {
		StringBuilder buf = new StringBuilder();
		DecimalFormat nf = NUMBER_FORMAT.get();
		buf.append("Executed ").append(nf.format(stats.errorCount + stats.successCount)).append(" queries in ")
				.append(stats.runtime.toString()).append(", ").append(nf.format(stats.successCount))
				.append(" were succesfull, ").append(nf.format(stats.errorCount)).append(" failed");
		String resp = buf.toString();
		LOG.info(resp);
		return resp;
	}

	private void executeImport(AjaxRequestTarget target, ImportOptions importOptions, FileUpload upload) {
		byte[] fileContentBytes = upload.getBytes();
		LOG.debug("Importing file of {} bytes", fileContentBytes.length);

		ImportResultWriter result = new ImportResultWriter();

		ImportConfig config = new ImportConfig();
		config.withContinueWithErrors(importOptions.isContinueWithErrors())
				.withUpdateHistory(importOptions.isContinueWithErrors());
		ImportStats stats = importer.importScript(new ByteArrayInputStream(fileContentBytes), result, config);

		String resp = createStatsMessage(stats);
		sendJsResponse(target, resp);

		importResultContainer.setVisible(true);
		target.add(importResultContainer);
		resultModel.setObject(result.getResult());

		updatePreferences(importOptions);

	}

	private Form<?> initFileUpload(final ImportOptions importOptions) {
		final FileUploadField scriptFile = new FileUploadField("scriptFile");

		final Form<?> form = new Form<Void>("form") {
			@Override
			protected void onSubmit() {
			}
		};
		form.setMaxSize(Bytes.megabytes(conf.queryImport.maxFileSizeMb));
		add(form);
		form.add(scriptFile);

		form.add(new AjaxButton("ajaxSubmit") {
			@Override
			protected void onError(AjaxRequestTarget target, Form<?> form) {
				LOG.info("Got error on import upload: {}", form);
				sendJsResponse(target, ErrorCodes.FILE_SIZE_LIMIT.name());
			}

			@Override
			protected void onSubmit(AjaxRequestTarget target, Form<?> form) {
				FileUpload upload = scriptFile.getFileUpload();
				if (upload == null) {
					return;
				}
				executeImport(target, importOptions, upload);
			}
		});
		return form;
	}

	private WebMarkupContainer initImportResultContainer() {
		WebMarkupContainer historyContainer = new WebMarkupContainer("importResultContainer");
		historyContainer.setOutputMarkupPlaceholderTag(true);
		historyContainer.setVisible(false);
		add(historyContainer);
		return historyContainer;
	}

	private ImmutableListModel<ImportResult> initResultTable(final WebMarkupContainer container) {
		ImmutableListModel<ImportResult> model = new ImmutableListModel<>();

		PageableListView<ImportResult> historyTable = new PageableListView<ImportResult>("resultRow", model, 1) {

			@Override
			protected void populateItem(ListItem<ImportResult> item) {
				ImportResult entry = item.getModel().getObject();

				populateRuntime(item, entry);
				populateQuery(item, entry);
			}
		};
		container.add(historyTable);
		final BootstrapPagingNavigator importResultPager = new BootstrapPagingNavigator("importResultPager",
				historyTable, new PagerConfigurator() {

			@Override
			public void onItemsPerPageChanged(AjaxRequestTarget target, long newItemsPerPage) {
				UserPreferences prefs = um.readPreferences().setPagerImportItems(newItemsPerPage);
				um.storePreferences(prefs);
			}

			@Override
			public long getInitialItemsPerPage() {
				return um.readPreferences().getPagerImportItems();
			}
		});
		container.add(importResultPager);

		model.registerOnChangeListener(new ModelChangeListener<ImportResult>() {
			@Override
			public void onModelChanged(ImmutableList<? extends ImportResult> object) {
				importResultPager.reset();
			}
		});

		return model;
	}

	private void initUploadOptions(Form<?> form) {
		final CheckBox continueWithErrors = new CheckBox("continueWithErrors");
		form.add(continueWithErrors);

		final CheckBox includeInHistory = new CheckBox("includeInHistory");
		form.add(includeInHistory);
	}

	private void populateQuery(ListItem<ImportResult> item, ImportResult entry) {
		Label queryLabel = new Label("query", entry.query.toDisplayString());
		queryLabel.setEscapeModelStrings(false);
		item.add(queryLabel);

		WebMarkupContainer errorContainer = new WebMarkupContainer("queryError");
		item.add(errorContainer);
		if (entry.error == null) {
			errorContainer.setVisible(false);
		} else {
			Label errorLabel = new Label("error", entry.error.getMessage());
			errorContainer.add(errorLabel);
		}
	}

	private void populateRuntime(ListItem<ImportResult> item, ImportResult entry) {
		String dateStr = Long.toString(entry.runTime);
		Label executedOn = new Label("runtime", dateStr);
		item.add(executedOn);
	}

	@Override
	public void renderHead(IHeaderResponse response) {
		super.renderHead(response);
		response.render(JavaScriptReferenceHeaderItem.forReference(JS_IMPORT));
	}

	private void sendJsResponse(AjaxRequestTarget target, String response) {
		String js = JsFunctionBuilder.function("onQueryImportResponse").param(response).build();
		target.appendJavaScript(js);
	}

	private void updatePreferences(ImportOptions importOptions) {
		UserPreferences prefs = um.readPreferences();
		prefs.setImportContinueWithErrors(importOptions.isContinueWithErrors());
		prefs.setImportIncludeInHistory(importOptions.isIncludeInHistory());
		um.storePreferences(prefs);
	}

}

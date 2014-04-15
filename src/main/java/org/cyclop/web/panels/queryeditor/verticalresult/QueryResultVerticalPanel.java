package org.cyclop.web.panels.queryeditor.verticalresult;

import com.datastax.driver.core.DataType;
import com.datastax.driver.core.Row;
import com.google.common.collect.ImmutableList;
import org.apache.wicket.AttributeModifier;
import org.apache.wicket.Component;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.injection.Injector;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.list.ListItem;
import org.apache.wicket.markup.html.list.ListView;
import org.apache.wicket.markup.html.list.PageableListView;
import org.apache.wicket.markup.html.panel.Panel;
import org.apache.wicket.model.IModel;
import org.cyclop.common.AppConfig;
import org.cyclop.model.CqlColumnType;
import org.cyclop.model.CqlDataType;
import org.cyclop.model.CqlExtendedColumnName;
import org.cyclop.model.CqlPartitionKey;
import org.cyclop.model.CqlQuery;
import org.cyclop.model.CqlQueryResult;
import org.cyclop.service.cassandra.QueryService;
import org.cyclop.web.components.column.WidgetFactory;
import org.cyclop.web.components.pagination.BootstrapPagingNavigator;

import javax.inject.Inject;
import java.util.ArrayList;
import java.util.List;

/** @author Maciej Miklas */
public class QueryResultVerticalPanel extends Panel {

	@Inject
	private QueryService queryService;

	private final RowsModel rowsModel;

	private final ColumnsModel columnsModel;

	@Inject
	private WidgetFactory widgetFactory;

	private final WebMarkupContainer resultTable;

	private final Label cqlResultText;

	private final CqlResultTextModel cqlResultTextModel;

	private final AppConfig appConfig = AppConfig.get();

	private BootstrapPagingNavigator pager;

	public QueryResultVerticalPanel(String id) {
		super(id);
		Injector.get().inject(this);

		cqlResultTextModel = new CqlResultTextModel();
		cqlResultText = new Label("cqlResultText", cqlResultTextModel);
		cqlResultText.setVisible(false);
		cqlResultText.setOutputMarkupPlaceholderTag(true);
		add(cqlResultText);

		resultTable = new WebMarkupContainer("resultTable");
		resultTable.setOutputMarkupPlaceholderTag(true);

		setOutputMarkupPlaceholderTag(true);
		add(resultTable);

		resultTable.setVisible(false);
		rowsModel = new RowsModel();
		columnsModel = new ColumnsModel();

		List<Row> displayedRows = initRowNamesList(resultTable, rowsModel, appConfig.cqlEditor.rowsPerPage);
		initColumnList(resultTable, columnsModel, displayedRows);
	}

	public CqlQueryResult executeQuery(CqlQuery query, AjaxRequestTarget target) {
		target.add(this);
		CqlQueryResult result = null;
		try {
			result = queryService.execute(query);
		} catch (Exception e) {
			showCqlResultText("CQL error: " + e.getMessage());
			return null;
		}

		if (result.isEmpty()) {
			showCqlResultText("Query executed successfully, result is empty");
		} else {
			showResultsTable(result);
		}
		return result;
	}

	private void hideCqlResultText() {
		cqlResultText.setVisible(false);
		cqlResultTextModel.clean();
	}

	private void hideResultsTable() {
		resultTable.setVisible(false);
		rowsModel.clean();
		columnsModel.clean();
	}

	private void initColumnList(WebMarkupContainer resultTable, ColumnsModel columnsModel,
								final List<Row> displayedRows) {
		ListView<CqlExtendedColumnName> columnList = new ListView<CqlExtendedColumnName>("columnList", columnsModel) {
			@Override
			protected void populateItem(ListItem<CqlExtendedColumnName> item) {
				final CqlExtendedColumnName columnName = item.getModelObject();

				WebMarkupContainer columnListRow = new WebMarkupContainer("columnListRow");
				item.add(columnListRow);

				Label columnNameLabel;
				if (columnName.columnType == CqlColumnType.SEPARATOR) {
					columnNameLabel = widgetFactory.createForSeparator("columnName");
					columnListRow.add(new AttributeModifier("class", new IModel<String>() {

						@Override
						public void detach() {
						}

						@Override
						public String getObject() {
							return "cq-tableRowSeparator";
						}

						@Override
						public void setObject(String object) {
						}
					}));
				} else {
					columnNameLabel = new Label("columnName", columnName.part);
				}
				columnListRow.add(columnNameLabel);

				ColumnsModel model = (ColumnsModel) getModel();
				CqlQueryResult result = model.getResult();
				final CqlPartitionKey partitionKey = result == null ? null : result.partitionKey;

				ListView<Row> columnValueList = new ListView<Row>("columnValueList", new RowsModel(displayedRows)) {

					@Override
					protected void populateItem(ListItem<Row> item) {
						Row row = item.getModelObject();

						Component component = widgetFactory
								.createForColumn(row, partitionKey, columnName, "columnValue");
						item.add(component);
						component.setRenderBodyOnly(true);
					}
				};
				columnListRow.add(columnValueList);
			}
		};
		resultTable.add(columnList);
	}

	private List<Row> initRowNamesList(WebMarkupContainer resultTable, RowsModel rowsModel, int perPage) {

		final List<Row> displayedRows = new ArrayList<>();
		PageableListView<Row> rowNamesList = new PageableListView<Row>("rowNamesList", rowsModel, perPage) {

			@Override
			protected void onBeforeRender() {
				displayedRows.clear();
				super.onBeforeRender();
			}

			@Override
			protected void populateItem(ListItem<Row> item) {
				Row row = item.getModel().getObject();
				displayedRows.add(row);

				RowsModel model = (RowsModel) getModel();
				CqlQueryResult result = model.getResult();
				CqlPartitionKey partitionKey = result.partitionKey;

				Component component;
				if (partitionKey != null) {
					component = widgetFactory.createForColumn(row, partitionKey, partitionKey, "rowName");
				} else {
					component = new Label("rowName", displayedRows.size());
				}

				item.add(component);
			}
		};
		resultTable.add(rowNamesList);

		pager = new BootstrapPagingNavigator("rowNamesListPager", rowNamesList);
		resultTable.add(pager);

		return displayedRows;
	}

	private void showCqlResultText(String text) {
		hideResultsTable();
		cqlResultText.setVisible(true);
		cqlResultTextModel.setObject(text);
	}

	private void showResultsTable(CqlQueryResult result) {
		hideCqlResultText();
		pager.reset();
		resultTable.setVisible(true);
		rowsModel.updateResult(result);
		columnsModel.updateResult(result);
	}

	private final static class ColumnsModel implements IModel<List<CqlExtendedColumnName>> {
		private CqlQueryResult result;

		private List<CqlExtendedColumnName> content = ImmutableList.of();

		public ColumnsModel() {
			this.content = ImmutableList.of();
		}

		public void clean() {
			this.content = ImmutableList.of();
			this.result = new CqlQueryResult();
		}

		@Override
		public void detach() {
		}

		@Override
		public List<CqlExtendedColumnName> getObject() {
			return content;
		}

		private CqlQueryResult getResult() {
			return result;
		}

		@Override
		public void setObject(List<CqlExtendedColumnName> object) {
			content = object;
		}

		public void updateResult(CqlQueryResult result) {
			this.result = result;
			ImmutableList.Builder<CqlExtendedColumnName> allColumnsBuild = ImmutableList.builder();
			allColumnsBuild.addAll(result.commonColumns);
			if (!result.commonColumns.isEmpty() && !result.dynamicColumns.isEmpty()) {
				allColumnsBuild
						.add(new CqlExtendedColumnName(CqlColumnType.SEPARATOR, CqlDataType.create(DataType.text()),
								"-"));
			}
			allColumnsBuild.addAll(result.dynamicColumns);

			List<CqlExtendedColumnName> allColumns = allColumnsBuild.build();
			setObject(allColumns);
		}
	}

	private final static class CqlResultTextModel implements IModel<String> {
		private String label = "";

		public void clean() {
			this.label = "";
		}

		@Override
		public void detach() {
		}

		@Override
		public String getObject() {
			return label;
		}

		@Override
		public void setObject(String label) {
			this.label = label;
		}
	}

	private final static class RowsModel implements IModel<List<Row>> {

		private List<Row> content;

		private CqlQueryResult result;

		public RowsModel() {
			this.content = ImmutableList.of();
		}

		public RowsModel(List<Row> content) {
			this.content = content;
		}

		public void clean() {
			this.content = ImmutableList.of();
			this.result = new CqlQueryResult();
		}

		@Override
		public void detach() {
		}

		@Override
		public List<Row> getObject() {
			return content;
		}

		private CqlQueryResult getResult() {
			return result;
		}

		@Override
		public void setObject(List<Row> object) {
			content = object;
		}

		public void updateResult(CqlQueryResult result) {
			this.result = result;
			setObject(result.rows);
		}
	}
}

package org.cyclop.web.pages.cqlcommander.verticalresult;

import com.datastax.driver.core.DataType;
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
import org.cyclop.model.*;
import org.cyclop.service.cassandra.QueryService;
import org.cyclop.web.components.pagination.BootstrapPagingNavigator;
import org.cyclop.web.pages.cqlcommander.column.WidgetFactory;

import javax.inject.Inject;
import java.util.ArrayList;
import java.util.List;

/**
 * @author Maciej Miklas
 */
public class QueryResultVerticalPanel extends Panel {

    @Inject
    private QueryService queryService;

    private final RowsModel rowsModel;

    private final ColumnsModel columnsModel;

    @Inject
    private WidgetFactory widgetFactory;

    private WebMarkupContainer resultTable;

    private Label cqlResultText;

    private CqlResultTextModel cqlResultTextModel;

    private AppConfig appConfig = AppConfig.get();

    public QueryResultVerticalPanel(String id) {
        super(id);
        Injector.get().inject(this);

        cqlResultTextModel = new CqlResultTextModel();
        cqlResultText = new Label("cqlResultText", cqlResultTextModel);
        cqlResultText.setVisible(false);
        cqlResultText.setOutputMarkupPlaceholderTag(true);
        cqlResultText.setOutputMarkupId(true);
        add(cqlResultText);

        resultTable = new WebMarkupContainer("resultTable");
        resultTable.setOutputMarkupPlaceholderTag(true);
        resultTable.setOutputMarkupId(true);

        setOutputMarkupPlaceholderTag(true);
        setOutputMarkupId(true);
        add(resultTable);

        resultTable.setVisible(false);
        rowsModel = new RowsModel();
        columnsModel = new ColumnsModel();

        List<CqlRow> displayedRows = initRowNamesList(resultTable, rowsModel, appConfig.cqlEditor.rowsPerPage);
        initColumnList(resultTable, columnsModel, displayedRows);
    }

    private void initColumnList(WebMarkupContainer resultTable, ColumnsModel columnsModel,
                                final List<CqlRow> displayedRows) {
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
                        public String getObject() {
                            return "cq-tableRowSeparator";
                        }

                        @Override
                        public void setObject(String object) {
                        }

                        @Override
                        public void detach() {
                        }
                    }));
                } else {
                    columnNameLabel = new Label("columnName", columnName.part);
                }
                columnListRow.add(columnNameLabel);

                ColumnsModel model = (ColumnsModel) getModel();
                CqlSelectResult result = model.getResult();
                final CqlPartitionKey partitionKey = result == null ? null : result.partitionKey;

                ListView<CqlRow> columnValueList = new ListView<CqlRow>("columnValueList",
                        new RowsModel(displayedRows)) {

                    @Override
                    protected void populateItem(ListItem<CqlRow> item) {
                        CqlRow row = item.getModelObject();

                        Component component = widgetFactory.createForColumn(row.original, partitionKey, columnName,
                                "columnValue");
                        item.add(component);
                        component.setRenderBodyOnly(true);
                    }
                };
                columnListRow.add(columnValueList);
            }
        };
        columnList.setRenderBodyOnly(true);
        resultTable.add(columnList);
    }

    private List<CqlRow> initRowNamesList(WebMarkupContainer resultTable, RowsModel rowsModel, int perPage) {

        final List<CqlRow> displayedRows = new ArrayList<>();
        PageableListView<CqlRow> rowNamesList = new PageableListView<CqlRow>("rowNamesList", rowsModel, perPage) {

            @Override
            protected void onBeforeRender() {
                displayedRows.clear();
                super.onBeforeRender();
            }

            @Override
            protected void populateItem(ListItem<CqlRow> item) {
                CqlRow row = item.getModel().getObject();
                displayedRows.add(row);

                RowsModel model = (RowsModel) getModel();
                CqlSelectResult result = model.getResult();
                CqlPartitionKey partitionKey = result.partitionKey;

                Component component;
                if (partitionKey != null) {
                    component = widgetFactory.createForColumn(row.original, partitionKey, partitionKey, "rowName");
                } else {
                    component = new Label("rowName", displayedRows.size());
                }

                item.add(component);
            }
        };
        resultTable.add(rowNamesList);

        BootstrapPagingNavigator pager = new BootstrapPagingNavigator("rowNamesListPager", rowNamesList);
        resultTable.add(pager);

        return displayedRows;
    }

    public CqlSelectResult executeQuery(CqlQuery query, AjaxRequestTarget target) {
        target.add(this);

        CqlSelectResult result = null;
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

    private void showCqlResultText(String text) {
        hideResultsTable();
        cqlResultText.setVisible(true);
        cqlResultTextModel.setObject(text);
    }

    private void hideResultsTable() {
        resultTable.setVisible(false);
        rowsModel.clean();
        columnsModel.clean();
    }

    private void showResultsTable(CqlSelectResult result) {
        hideCqlResultText();
        resultTable.setVisible(true);
        rowsModel.updateResult(result);
        columnsModel.updateResult(result);
    }

    private final static class RowsModel implements IModel<List<CqlRow>> {

        private List<CqlRow> content;

        private CqlSelectResult result;

        public void clean() {
            this.content = ImmutableList.of();
            this.result = new CqlSelectResult();
        }

        @Override
        public List<CqlRow> getObject() {
            return content;
        }

        public RowsModel(List<CqlRow> content) {
            this.content = content;
        }

        public RowsModel() {
            this.content = ImmutableList.of();
        }

        @Override
        public void setObject(List<CqlRow> object) {
            content = object;
        }

        public void updateResult(CqlSelectResult result) {
            this.result = result;
            setObject(result.rows);
        }

        private CqlSelectResult getResult() {
            return result;
        }

        @Override
        public void detach() {
        }
    }

    private final static class ColumnsModel implements IModel<List<CqlExtendedColumnName>> {
        private CqlSelectResult result;

        private List<CqlExtendedColumnName> content = ImmutableList.of();

        public ColumnsModel() {
            this.content = ImmutableList.of();
        }

        public void clean() {
            this.content = ImmutableList.of();
            this.result = new CqlSelectResult();
        }

        @Override
        public List<CqlExtendedColumnName> getObject() {
            return content;
        }

        @Override
        public void setObject(List<CqlExtendedColumnName> object) {
            content = object;
        }

        public void updateResult(CqlSelectResult result) {
            this.result = result;
            ImmutableList.Builder<CqlExtendedColumnName> allColumnsBuild = ImmutableList.builder();
            List<CqlExtendedColumnName> allColumns = allColumnsBuild.addAll(result.commonColumns).add(new
                    CqlExtendedColumnName(CqlColumnType.SEPARATOR, DataType.text(),
                    "-")).addAll(result.dynamicColumns).build();
            setObject(allColumns);
        }

        private CqlSelectResult getResult() {
            return result;
        }

        @Override
        public void detach() {
        }
    }

    private final static class CqlResultTextModel implements IModel<String> {
        private String label = "";

        @Override
        public String getObject() {
            return label;
        }

        @Override
        public void setObject(String label) {
            this.label = label;
        }

        @Override
        public void detach() {
        }

        public void clean() {
            this.label = "";
        }
    }
}

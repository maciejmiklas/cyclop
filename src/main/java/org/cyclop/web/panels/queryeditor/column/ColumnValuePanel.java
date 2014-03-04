package org.cyclop.web.panels.queryeditor.column;

import org.apache.wicket.AttributeModifier;
import org.apache.wicket.Component;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.AjaxFallbackLink;
import org.apache.wicket.injection.Injector;
import org.apache.wicket.markup.ComponentTag;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.panel.Panel;
import org.apache.wicket.model.IModel;
import org.cyclop.model.CqlColumnValue;
import org.cyclop.model.CqlExtendedColumnName;
import org.cyclop.model.CqlPartitionKeyValue;
import org.cyclop.service.converter.DataConverter;
import org.cyclop.web.components.infodialog.InfoDialog;

import javax.inject.Inject;

/** @author Maciej Miklas */
class ColumnValuePanel extends Panel {

	private final InfoDialog infoDialog;

	@Inject
	private DataConverter converter;

	ColumnValuePanel(String componentId, final CqlPartitionKeyValue cqlPartitionKeyValue,
					 final CqlColumnValue cqlColumnValue, boolean embeddedColumn) {
		super(componentId);
		Injector injector = Injector.get();
		injector.inject(this);

		String convertedValue = converter.convert(cqlColumnValue.value);
		final String convertedValueNotNull = convertedValue == null ? "" : convertedValue;
		final String trimmedEntry = converter.trimColumnContent(convertedValueNotNull, embeddedColumn);
		boolean trimmed = convertedValueNotNull.length() - trimmedEntry.length() > 10;

		infoDialog = new InfoDialog("columnContentDialog");
		infoDialog.setVisible(trimmed);
		add(infoDialog);

		Component fullContentLink;
		Label columnContent;
		if (trimmed) {
			fullContentLink = new AjaxFallbackLink<Object>("columnContentLink") {
				@Override
				public void onClick(AjaxRequestTarget target) {

					String title = crateInfoDialogTitle(cqlPartitionKeyValue, cqlColumnValue.columnName);
					infoDialog.open(target, this.getMarkupId(), title, convertedValueNotNull);
				}
			};

			columnContent = new Label("columnContent", trimmedEntry);
			fullContentLink.add(new AttributeModifier("title", new IModel<String>() {

				@Override
				public String getObject() {
					String conv = converter.trimColumnTooltipContent(convertedValueNotNull);
					return conv;
				}

				@Override
				public void setObject(String object) {
				}

				@Override
				public void detach() {
				}
			}));

		} else {
			fullContentLink = new Label("columnContentLink", "") {
				@Override
				protected void onComponentTag(ComponentTag tag) {
					if ("a".equalsIgnoreCase(tag.getName())) {
						tag.setName("div");
					}
					super.onComponentTag(tag);
				}
			};
			columnContent = new Label("columnContent", convertedValueNotNull);
			fullContentLink.setRenderBodyOnly(true);
		}
		add(fullContentLink);
		add(columnContent);
	}

	private String crateInfoDialogTitle(CqlPartitionKeyValue cqlPartitionKeyValue, CqlExtendedColumnName columnName) {
		String partitionKeyValue = cqlPartitionKeyValue == null ? null : converter.convert(cqlPartitionKeyValue.value);
		return (partitionKeyValue == null ? "Key" : partitionKeyValue) + " -> " + columnName.toDisplayString();
	}
}

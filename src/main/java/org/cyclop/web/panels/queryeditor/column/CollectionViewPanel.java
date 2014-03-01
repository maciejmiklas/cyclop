package org.cyclop.web.panels.queryeditor.column;

import com.google.common.collect.ImmutableList;
import org.apache.wicket.Component;
import org.apache.wicket.markup.ComponentTag;
import org.apache.wicket.markup.html.list.ListItem;
import org.apache.wicket.markup.html.list.ListView;
import org.apache.wicket.markup.html.panel.Panel;
import org.cyclop.model.CqlColumnValue;
import org.cyclop.model.CqlPartitionKeyValue;

/** @author Maciej Miklas */
class CollectionViewPanel extends Panel {

	CollectionViewPanel(String id, final CqlPartitionKeyValue cqlPartitionKeyValue,
						final ImmutableList<CqlColumnValue> content) {
		super(id);

		ListView<CqlColumnValue> cqlCollectionListEntry = new ListView<CqlColumnValue>("listEntry", content) {
			@Override
			protected void populateItem(ListItem<CqlColumnValue> item) {
				CqlColumnValue cqlColumnValue = item.getModelObject();
				Component entry = new ColumnValuePanel("entryValue", cqlPartitionKeyValue, cqlColumnValue, true);
				item.add(entry);
			}
		};
		add(cqlCollectionListEntry);
	}

	@Override
	protected void onComponentTag(ComponentTag tag) {
		if ("a".equalsIgnoreCase(tag.getName())) {
			tag.setName("div");
		}
		super.onComponentTag(tag);
	}
}

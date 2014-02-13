package org.cyclop.web.pages.cqlcommander.column;

import com.google.common.collect.ImmutableMap;
import org.apache.wicket.Component;
import org.apache.wicket.markup.ComponentTag;
import org.apache.wicket.markup.html.list.ListItem;
import org.apache.wicket.markup.html.list.ListView;
import org.apache.wicket.markup.html.panel.Panel;
import org.cyclop.model.CqlColumnValue;
import org.cyclop.model.CqlPartitionKeyValue;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

/** @author Maciej Miklas */
class MapViewPanel extends Panel {

	MapViewPanel(String id, final CqlPartitionKeyValue cqlPartitionKeyValue,
				 final ImmutableMap<CqlColumnValue, CqlColumnValue> content) {
		super(id);

		List<Map.Entry<CqlColumnValue, CqlColumnValue>> keysList = new ArrayList<>(content.size());
		keysList.addAll(content.entrySet());

		ListView<Map.Entry<CqlColumnValue, CqlColumnValue>> cqlCollectionListEntry = new ListView<Map.Entry<CqlColumnValue, CqlColumnValue>>(
				"cqlMapEntry", keysList) {
			@Override
			protected void populateItem(ListItem<Map.Entry<CqlColumnValue, CqlColumnValue>> item) {
				Map.Entry<CqlColumnValue, CqlColumnValue> entry = item.getModelObject();
				CqlColumnValue key = entry.getKey();
				CqlColumnValue value = entry.getValue();

				Component cqlMapKeyComp = new ColumnValuePanel("cqlMapKey", cqlPartitionKeyValue, key, true);
				item.add(cqlMapKeyComp);

				Component cqlMapValueComp = new ColumnValuePanel("cqlMapValue", cqlPartitionKeyValue, value, true);
				item.add(cqlMapValueComp);
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

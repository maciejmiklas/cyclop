/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.cyclop.web.components.column;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.apache.wicket.Component;
import org.apache.wicket.markup.ComponentTag;
import org.apache.wicket.markup.html.list.ListItem;
import org.apache.wicket.markup.html.list.ListView;
import org.apache.wicket.markup.html.panel.Panel;
import org.cyclop.model.CqlColumnValue;
import org.cyclop.model.CqlPartitionKeyValue;

import com.google.common.collect.ImmutableMap;

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

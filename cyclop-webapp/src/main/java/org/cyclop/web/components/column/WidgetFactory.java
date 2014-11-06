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

import java.util.Optional;

import javax.inject.Inject;
import javax.inject.Named;
import javax.validation.constraints.NotNull;

import org.apache.wicket.Component;
import org.apache.wicket.behavior.AttributeAppender;
import org.apache.wicket.markup.html.basic.Label;
import org.cyclop.model.CqlColumnValue;
import org.cyclop.model.CqlDataType;
import org.cyclop.model.CqlExtendedColumnName;
import org.cyclop.model.CqlPartitionKey;
import org.cyclop.model.CqlPartitionKeyValue;
import org.cyclop.service.converter.DataConverter;
import org.cyclop.service.converter.DataExtractor;
import org.cyclop.validation.EnableValidation;

import com.datastax.driver.core.DataType;
import com.datastax.driver.core.Row;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableMap;

/** @author Maciej Miklas */
@Named
@EnableValidation
public class WidgetFactory {

	@Inject
	private DataExtractor extractor;

	protected WidgetFactory() {
	}

	private Component createForCollection(Row row, CqlPartitionKeyValue cqlPartitionKeyValue,
			CqlExtendedColumnName column, String componentId) {
		ImmutableList<CqlColumnValue> content = extractor.extractCollection(row, column);
		Component comp;
		if (content.isEmpty()) {
			comp = createForEmptyColumn(componentId);
		} else {
			comp = new CollectionViewPanel(componentId, cqlPartitionKeyValue, content);
		}

		return comp;
	}

	public void addColumnTitle(Component colValue, Optional<CqlPartitionKeyValue> partitionKey,
			CqlExtendedColumnName column) {
		StringBuilder title = new StringBuilder();
		if (partitionKey.isPresent()) {
			title.append(partitionKey.get().value);
			title.append(" -> ");
		}
		title.append(column.part);

		colValue.add(new AttributeAppender("title", title.toString()));
	}

	public @NotNull Component createColumnValue(@NotNull Row row, @NotNull Optional<CqlPartitionKey> partitionKey,
			@NotNull CqlExtendedColumnName column, @NotNull String componentId) {

		Component colValue = createForDataType(row, partitionKey, column, componentId).orElse(
				createForEmptyColumn(componentId));
		return colValue;
	}

	private Optional<Component> createForDataType(Row row, Optional<CqlPartitionKey> partitionKey,
			CqlExtendedColumnName column,			String componentId) {
		String partLc = column.partLc;
		if (row.isNull(partLc)) {
			return Optional.empty();
		}

		CqlPartitionKeyValue cqlPartitionKeyValue = null;
		if (partitionKey.isPresent()) {
			cqlPartitionKeyValue = extractor.extractPartitionKey(row, partitionKey.get());
		}

		Component component;
		CqlDataType dataType = column.dataType;
		if (dataType.name == DataType.Name.SET || dataType.name == DataType.Name.LIST) {
			component = createForCollection(row, cqlPartitionKeyValue, column, componentId);

		} else if (dataType.name == DataType.Name.MAP) {
			component = createForMap(row, cqlPartitionKeyValue, column, componentId);

		} else {
			component = createForSingleValue(row, cqlPartitionKeyValue, column, componentId);
		}
		return Optional.of(component);
	}

	private Label createForEmptyColumn(String componentId) {
		return new Label(componentId, DataConverter.EMPTY_COL_VALUE);
	}

	private Component createForMap(Row row, CqlPartitionKeyValue cqlPartitionKeyValue, CqlExtendedColumnName column,
			String componentId) {
		ImmutableMap<CqlColumnValue, CqlColumnValue> displayMap = extractor.extractMap(row, column);
		Component comp;
		if (displayMap.isEmpty()) {
			comp = createForEmptyColumn(componentId);
		} else {
			comp = new MapViewPanel(componentId, cqlPartitionKeyValue, displayMap);
		}

		return comp;
	}

	private Component createForSingleValue(Row row, CqlPartitionKeyValue cqlPartitionKeyValue,
			CqlExtendedColumnName column, String componentId) {
		CqlColumnValue cqlColumnValue = extractor.extractSingleValue(row, column);
		Component component = new ColumnValuePanel(componentId, cqlPartitionKeyValue, cqlColumnValue, false);
		return component;
	}

}

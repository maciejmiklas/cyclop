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
package org.cyclop.service.converter;

import org.cyclop.common.AppConfig;
import org.joda.time.DateTime;
import org.joda.time.format.DateTimeFormat;
import org.joda.time.format.DateTimeFormatter;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.inject.Inject;
import javax.inject.Named;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.net.InetAddress;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.UUID;

/** @author Maciej Miklas */
@Named
public class DataConverter {

	private final ThreadLocal<SimpleDateFormat> dateFomrat;

	private final DateTimeFormatter timeFormatter;

	public final static String EMPTY_COL_VALUE = "";

	private final static Logger LOG = LoggerFactory.getLogger(DataConverter.class);

	private final static String DATE_PATTERN = "yyyy-MM-dd HH:mm:ss.SSS";

	@Inject
	private AppConfig appConfig;

	protected DataConverter() {
		dateFomrat = new ThreadLocal<SimpleDateFormat>() {
			@Override
			protected SimpleDateFormat initialValue() {
				return new SimpleDateFormat(DATE_PATTERN);
			}
		};

		timeFormatter = DateTimeFormat.forPattern(DATE_PATTERN);
	}

	public String trimColumnContent(String content, boolean embeddedColumn) {
		if (content == null) {
			return null;
		}

		int limit = embeddedColumn ? appConfig.queryEditor.maxColumnEmbeddedDisplayChars : appConfig.queryEditor.maxColumnDisplayChars;
		if (content.length() > limit) {
			return content.trim().substring(0, limit) + ".....";
		} else {
			return content.trim();
		}
	}

	public String trimColumnTooltipContent(String content) {
		if (content == null) {
			return null;
		}

		if (content.length() > appConfig.queryEditor.maxColumnTooltipDisplayChars) {
			return content.trim().substring(0, appConfig.queryEditor.maxColumnTooltipDisplayChars) + ".....";
		} else {
			return content.trim();
		}
	}

	public String convert(Object val) {
		if (val == null) {
			return null;
		}
		String converted;
		if (val instanceof String) {
			converted = val.toString();

		} else if (val instanceof InetAddress) {
			converted = val.toString();

		} else if (val instanceof UUID) {
			converted = val.toString();

		} else if (val instanceof Date) {
			converted = dateFomrat.get().format(val);

		} else if (val instanceof DateTime) {
			converted = timeFormatter.print((DateTime) val);

		} else if (val instanceof BigInteger) {
			converted = val.toString();

		} else if (val instanceof Double) {
			converted = Double.toString((Double) val);

		} else if (val instanceof BigDecimal) {
			converted = val.toString();

		} else if (val instanceof Boolean) {
			converted = Boolean.toString((Boolean) val);

		} else if (val instanceof Integer) {
			converted = Integer.toString((Integer) val);

		} else if (val instanceof Long) {
			converted = Long.toString((Long) val);

		} else if (val instanceof Float) {
			converted = Float.toString((Float) val);

		} else {
			LOG.warn("Could not find mapping for type: {}", val.getClass());
			converted = val.toString();
		}

		LOG.trace("Converted: {} to {}", val, converted);
		return converted;
	}

}

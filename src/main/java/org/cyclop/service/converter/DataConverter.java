package org.cyclop.service.converter;

import org.cyclop.common.AppConfig;
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
	public final static String EMPTY_COL_VALUE = "";

	private final static Logger LOG = LoggerFactory.getLogger(DataConverter.class);

	private final static String DATE_PATTERN = "yyyy-MM-dd HH:mm:ss.SSS";

	@Inject
	private AppConfig appConfig;

	protected DataConverter() {

	}

	public String trimColumnContent(String content, boolean embeddedColumn) {
		if (content == null) {
			return null;
		}

		int limit = embeddedColumn ? appConfig.cqlEditor.maxColumnEmbeddedDisplayChars : appConfig.cqlEditor
				.maxColumnDisplayChars;
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

		if (content.length() > appConfig.cqlEditor.maxColumnTooltipDisplayChars) {
			return content.trim().substring(0, appConfig.cqlEditor.maxColumnTooltipDisplayChars) + ".....";
		} else {
			return content.trim();
		}
	}

	public String convert(Object val) {
		if (val == null) {
			return null;
		}
		String converted = null;
		if (val instanceof String) {
			converted = val.toString();

		} else if (val instanceof InetAddress) {
			converted = val.toString();

		} else if (val instanceof UUID) {
			converted = val.toString();

		} else if (val instanceof Date) {
			SimpleDateFormat formatter = new SimpleDateFormat(DATE_PATTERN);
			converted = formatter.format(val);

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
		return converted;
	}

}

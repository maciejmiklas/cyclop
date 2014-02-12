package org.cyclop.model.adapter;

import org.apache.commons.lang.StringUtils;
import org.joda.time.DateTime;

import javax.xml.bind.annotation.adapters.XmlAdapter;

/** @author Maciej Miklas */
public class DateAdapter extends XmlAdapter<String, DateTime> {


	@Override
	public DateTime unmarshal(String val) throws Exception {
		val = StringUtils.trimToNull(val);
		if (val == null) {
			return null;
		}
		return DateTime.parse(val);
	}

	@Override
	public String marshal(DateTime val) throws Exception {
		if (val == null) {
			return null;
		}
		return val.toString();
	}
}

package org.cyclop.model.adapter;

import javax.xml.bind.annotation.adapters.XmlAdapter;

/** @author Maciej Miklas */
public class BooleanDefaultTrueAdapter extends XmlAdapter<String, Boolean> {
	@Override
	public Boolean unmarshal(String val) throws Exception {
		return val == null || "1".equals(val.trim());
	}

	@Override
	public String marshal(Boolean val) throws Exception {
		if (val == null) {
			return null;
		}
		return val ? "1" : "0";
	}
}

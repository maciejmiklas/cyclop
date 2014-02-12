@XmlJavaTypeAdapters({
		@XmlJavaTypeAdapter(value = DateAdapter.class, type = DateTime.class)
}) package org.cyclop.model;

import org.cyclop.model.adapter.DateAdapter;
import org.joda.time.DateTime;

import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapters;

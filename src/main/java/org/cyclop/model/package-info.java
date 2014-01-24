@XmlJavaTypeAdapters({
        @XmlJavaTypeAdapter(value = DateAdapter.class, type = DateTime.class)
}) package org.cyclop.model;

import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapters;
import org.cyclop.model.adapter.DateAdapter;
import org.joda.time.DateTime;
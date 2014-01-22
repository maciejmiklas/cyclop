@XmlJavaTypeAdapters({
        @XmlJavaTypeAdapter(value = DateAdapter.class, type = Date.class)
}) package org.cyclop.model;

import org.cyclop.model.adapter.DateAdapter;

import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapters;
import java.util.Date;
package org.cyclop.model.adapter;

import org.apache.commons.lang.StringUtils;

import javax.xml.bind.annotation.adapters.XmlAdapter;
import java.text.SimpleDateFormat;
import java.util.Date;

/**
 * @author Maciej Miklas
 */
public class DateAdapter extends XmlAdapter<String, Date> {

    private final static ThreadLocal<SimpleDateFormat> FORMAT = new ThreadLocal<SimpleDateFormat>() {
        @Override
        protected SimpleDateFormat initialValue() {
            SimpleDateFormat sid = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ssZ");
            return sid;
        }
    };

    @Override
    public Date unmarshal(String val) throws Exception {
        val = StringUtils.trimToNull(val);
        if (val == null) {
            return null;
        }
        return FORMAT.get().parse(val);
    }

    @Override
    public String marshal(Date val) throws Exception {
        if (val == null) {
            return null;
        }
        return FORMAT.get().format(val);
    }
}

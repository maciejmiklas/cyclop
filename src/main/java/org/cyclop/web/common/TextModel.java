package org.cyclop.web.common;

import org.apache.wicket.model.util.GenericBaseModel;

/** @author Maciej Miklas */
public final class TextModel extends GenericBaseModel<String>{

    @Override
    protected String createSerializableVersionOf(String object) {
	return object;
    }

}

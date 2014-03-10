package org.cyclop.web.common;

import com.google.common.collect.ImmutableList;
import org.apache.wicket.model.util.GenericBaseModel;

/** @author Maciej Miklas */
public class ImmutableListModel<T> extends GenericBaseModel<ImmutableList<? extends T>> {

	public ImmutableListModel() {
		ImmutableList<? extends T> empty = ImmutableList.of();
		setObject(empty);
	}

	public ImmutableListModel(ImmutableList<T> data) {
		setObject(data);
	}

	@Override
	protected final ImmutableList<? extends T> createSerializableVersionOf(ImmutableList<? extends T> object) {
		return object;
	}
}

package org.cyclop.web.common;

import org.apache.wicket.model.IModel;

/** @author Maciej Miklas */
public final class TransientModel<T> implements IModel<T> {

	private transient T object;

	public TransientModel(T obj) {
		setObject(obj);
	}

	@Override
	public T getObject() {
		return object;
	}

	@Override
	public void setObject(T object) {
		this.object = object;
	}

	@Override
	public void detach() {
		object = null;
	}
}

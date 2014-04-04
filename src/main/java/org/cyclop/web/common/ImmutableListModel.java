package org.cyclop.web.common;

import java.util.ArrayList;
import java.util.List;

import org.apache.wicket.model.util.GenericBaseModel;

import com.google.common.collect.ImmutableList;

/** @author Maciej Miklas */
public class ImmutableListModel<T> extends GenericBaseModel<ImmutableList<? extends T>> {

    private final List<ModelChangeListener<T>> listeners = new ArrayList<>();

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

    public void registerOnChangeListener(ModelChangeListener<T> listener) {
	listeners.add(listener);
    }

    @Override
    public void setObject(ImmutableList<? extends T> object) {
	super.setObject(object);

	for (ModelChangeListener<T> list : listeners) {
	    list.onModelChanged(object);
	}
    }

    public interface ModelChangeListener<T> {
	void onModelChanged(ImmutableList<? extends T> object);
    }
}

package org.cyclop.web.components.iterablegrid;

/** @author Maciej Miklas */
public abstract class IterableGridView<T> extends GridView<T> {

	private final IterableDataProvider<T> dataProvider;

	public IterableGridView(String id, IterableDataProvider<T> dataProvider) {
		super(id, dataProvider);
		this.dataProvider = dataProvider;
	}

	@Override
	public void setCurrentPage(long page) {
		dataProvider.setCurrentPage(page);
		super.setCurrentPage(page);
	}
}

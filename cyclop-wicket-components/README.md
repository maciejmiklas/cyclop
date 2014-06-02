# Overview
This project contains wicket components. It has been tested with wicket 6.15.0

# Components

## Bootstrap Paging Navigator
Package: `org.cyclop.web.components.pagination`

This is Wicket's `AjaxPagingNavigator` which view has been adopted to Bootstrap 3 HTML structure. It also supports Iterable Grid View.

This blog article contains implementation details: http://maciej-miklas.blogspot.de/2013/10/wicket-6-paging-navigator-for-bootstrap.html

## Iterable Grid View
Package: `org.cyclop.web.components.iterablegrid`

Iterable Grid View is based on Wicket's `GridView`, however it does not work with `IDataProvider` but with
 `IterableDataProvider`. This new data provider relies only on plain java iterator - size information is not needed, and there is also no need to create range iterators for each page.

``` java
final List<String> myGridData = new ArrayList<>();
myGridData.add("value 1");
myGridData.add("value 2");

IterableDataProvider<String> iterableDataProvider = new IterableDataProvider<String>(10) {
	@Override
	protected Iterator<String> iterator() {
		return myGridData.iterator();
	}

	@Override
	public IModel<String> model(String s) {
		return Model.of(s);
	}

	@Override
	public void detach() {
	}
};

IterableGridView<String> myGrid = new IterableGridView<String>("myGrid", iterableDataProvider) {
	@Override
	protected void populateEmptyItem(Item<String> item) {
		item.add(new Label("myValue"));
	}

	@Override
	protected void populateItem(Item<String> item) {
		item.add(new Label("myValue", item.getModelObject()));
	}
};

add(myGrid);

myGrid.setItemsPerPage(10);

// you have to use custom pager and not AjaxPagingNavigator
IterablePagingNavigator pager = new IterablePagingNavigator("rowNamesListPager", rowNamesList);
resultTable.add(pager);

```

Here you can find implementation example: https://github.com/maciejmiklas/cyclop/tree/master/cyclop-webapp/src/main/java/org/cyclop/web/panels/queryeditor/verticalresult

See also this linkg for more details: http://maciej-miklas.blogspot.de/2014/05/wickets-data-grid-based-on-plain.html


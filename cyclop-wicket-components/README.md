# Overview
This project contains wicket components. It has been tested with wicket 6.15.0

# Components

## Bootstrap Paging Navigator
Package: `org.cyclop.web.components.pagination`

This is Wicket's `AjaxPagingNavigator` which view has been adopted to Bootstrap 3 HTML structure. This blog article contains implementation details: http://maciej-miklas.blogspot.de/2013/10/wicket-6-paging-navigator-for-bootstrap.html

## Iterable Grid View
Package: `org.cyclop.web.components.iterablegrid`

This is the Wicket's `GridView` that does not work with `IDataProvider` but with `IterableDataProvider`. New data  provider has a bit different interface - it requires only plain Iterator and a factory method to create models for elements  returned by the iterator:

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
		// do something ....
	}

	@Override
	protected void populateItem(Item<String> components) {
		// do something ....
	}
};

```



Here you can find implementation example: https://github.com/maciejmiklas/cyclop/tree/master/cyclop-webapp/src/main/java/org/cyclop/web/panels/queryeditor/verticalresult



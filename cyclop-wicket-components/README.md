# Overview
This project contains wicket components. It has been tested with wicket 6.15.0

# Bootstrap Paging Navigator
This is Wicket's `AjaxPagingNavigator` which view has been adopted to Bootstrap 3 HTML structure. This blog article contains implementation details: http://maciej-miklas.blogspot.de/2013/10/wicket-6-paging-navigator-for-bootstrap.html

# Iterable Grid View
This is the Wicket's `GridView` that does not work with `IDataProvider` but with `IterableDataProvider`. New data  provider has a bit different interface - it requires only Iterator and factory method to create models for elements  returned by the iterator.

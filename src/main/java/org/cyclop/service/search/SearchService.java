package org.cyclop.service.search;

import org.cyclop.model.FilterResult;

import com.google.common.collect.ImmutableCollection;

public interface SearchService<T> {

    FilterResult<T> filter(ImmutableCollection<T> input, FilterFieldAccessor<T> accessor, String... keywords);
}

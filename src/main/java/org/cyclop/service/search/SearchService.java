package org.cyclop.service.search;

import com.google.common.collect.ImmutableCollection;
import org.cyclop.model.FilterResult;

public interface SearchService<T> {

	FilterResult<T> filter(ImmutableCollection<T> input, FilterFieldAccessor<T> accessor, String... keywords);
}

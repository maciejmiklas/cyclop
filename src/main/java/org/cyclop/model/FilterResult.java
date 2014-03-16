package org.cyclop.model;

import java.io.Serializable;

import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableSet;

/** @author Maciej Miklas */
public class FilterResult<T> implements Serializable {

    public final ImmutableList<T> result;

    /** trimmed, lower case */
    public final ImmutableSet<String> normalizedKeywords;

    public FilterResult(ImmutableList<T> result, ImmutableSet<String> normalizedKeywords) {
	this.result = result;
	this.normalizedKeywords = normalizedKeywords;
    }

    @Override
    public String toString() {
	return "FilterResult [result=" + result + ", normalizedKeywords=" + normalizedKeywords + "]";
    }

}

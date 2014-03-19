package org.cyclop.service.search.impl;

import com.google.common.collect.ImmutableCollection;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableSet;
import org.cyclop.model.FilterResult;
import org.cyclop.service.search.FilterFieldAccessor;
import org.cyclop.service.search.SearchService;
import org.cyclop.validation.EnableValidation;

import javax.inject.Named;
import javax.validation.constraints.NotNull;
import java.util.Comparator;
import java.util.Set;
import java.util.SortedMap;
import java.util.TreeMap;

@Named
@EnableValidation
public class SearchServiceImpl<T> implements SearchService<T> {

	private final static WeightSortingComparator COMP = new WeightSortingComparator();

	private final static int MIN_KW_LENGHT = 3;

	@Override
	public
	@NotNull
	FilterResult<T> filter(@NotNull ImmutableCollection<T> input, @NotNull FilterFieldAccessor<T> accessor,
						   @NotNull String... keywords) {

		ImmutableSet<String> normKeywords = normalize(keywords);
		if (normKeywords.isEmpty()) {
			return null;
		}

		SortedMap<WeightSortingKey, T> sorted = new TreeMap<>(COMP);
		for (T el : input) {
			String elVal = accessor.getText(el);
			if (elVal == null) {
				continue;
			}
			String normElVal = normalize(elVal);
			int weigth = calculateWeight(normElVal, normKeywords);
			if (weigth == 0) {
				continue;
			}
			sorted.put(new WeightSortingKey(weigth, normElVal), el);
		}

		ImmutableList<T> result = mapResult(sorted);
		return new FilterResult<T>(result, normKeywords);
	}

	private ImmutableList<T> mapResult(SortedMap<WeightSortingKey, T> sorted) {
		ImmutableList.Builder<T> resList = ImmutableList.builder();
		for (T val : sorted.values()) {
			resList.add(val);
		}
		return resList.build();
	}

	// TODO !!!! remove larger keywords if they contain shorter ones
	private ImmutableSet<String> normalize(String... strArray) {
		ImmutableSet.Builder<String> resSet = ImmutableSet.builder();
		for (int idx = 0; idx < strArray.length; idx++) {
			String normalized = normalize(strArray[idx]);
			if (normalized.length() < MIN_KW_LENGHT) {
				continue;
			}
			resSet.add(normalized);
		}
		return resSet.build();
	}

	private String normalize(String str) {
		return str.trim().toLowerCase();
	}

	private int calculateWeight(String valueLc, Set<String> keywordsLc) {
		int weight = 0;
		for (String kw : keywordsLc) {
			if (valueLc.contains(kw)) {
				weight++;
			}
		}
		return weight;
	}

	private final static class WeightSortingKey {
		public final int weight;

		public final String keyLc;

		public WeightSortingKey(int weight, String keyLc) {
			super();
			this.weight = weight;
			this.keyLc = keyLc;
		}

		@Override
		public String toString() {
			return "WeightSortingKey [weight=" + weight + ", keyLc=" + keyLc + "]";
		}

	}

	public final static class WeightSortingComparator implements Comparator<WeightSortingKey> {
		@Override
		public int compare(WeightSortingKey o1, WeightSortingKey o2) {
			int compRes = 0;
			if (o1.weight == o2.weight) {
				compRes = o1.keyLc.compareTo(o2.keyLc);
			} else {
				compRes = o2.weight - o1.weight;
			}
			return compRes;
		}
	}

}

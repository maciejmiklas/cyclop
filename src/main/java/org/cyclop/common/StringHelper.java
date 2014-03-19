package org.cyclop.common;

import java.util.Comparator;
import java.util.Set;
import java.util.TreeSet;

/** @author Maciej Miklas */
public class StringHelper {

	public static String decorate(String toDecorate, StringDecorator decorator, String... keywordsLc) {

		int prefixLength = decorator.prefix().length();
		StringBuilder buf = new StringBuilder(toDecorate);
		for (String keyword : sortBySize(keywordsLc)) {

			String bufLc = buf.toString().toLowerCase();
			int startSearch = 0;
			int foundIdx = -1;
			while ((foundIdx = bufLc.indexOf(keyword, startSearch)) >= 0) {
				if (foundIdx > prefixLength) {
					String kwWithPref = bufLc.substring(foundIdx - prefixLength, foundIdx);
					if (decorator.prefix().equals(kwWithPref)) {
						startSearch = foundIdx + kwWithPref.length() + keyword.length() + 1;
						// word already replaced with longer one (better match)
						continue;
					}
				}
				int kwStart = foundIdx;
				int kwEnd = foundIdx + keyword.length();
				String orgKw = buf.substring(kwStart, kwEnd);
				String decoratedKw = decorator.decorate(orgKw);
				buf.replace(kwStart, kwEnd, decoratedKw);

				bufLc = buf.toString().toLowerCase();
				startSearch = foundIdx + decoratedKw.length() + 1;
			}
		}
		return buf.toString();
	}

	public static interface StringDecorator {
		String decorate(String in);

		String prefix();
	}

	private static Set<String> sortBySize(String... strArray) {
		Set<String> sorted = new TreeSet<>(new Comparator<String>() {
			@Override
			public int compare(String o1, String o2) {
				int lengthDif = o2.length() - o1.length();
				if (lengthDif != 0) {
					return lengthDif;
				}
				return o1.compareTo(o2);
			}
		});

		for (String str : strArray) {
			sorted.add(str);
		}
		return sorted;
	}
}

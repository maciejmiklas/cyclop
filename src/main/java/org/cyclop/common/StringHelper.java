package org.cyclop.common;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Comparator;
import java.util.Set;
import java.util.TreeSet;

/** @author Maciej Miklas */
public class StringHelper {
	private final static Logger LOG = LoggerFactory.getLogger(StringHelper.class);

	public static String decorate(String toDecorate, StringDecorator decorator, String... keywordsLc) {

		String prefix = decorator.prefix();
		String postfix = decorator.postfix();

		int prefixLength = prefix.length();
		StringBuilder buf = new StringBuilder(toDecorate);
		for (String keyword : sortBySize(keywordsLc)) {
			if (prefix.contains(keyword) || postfix.contains(keyword)) {
				LOG.debug("Skipping keyword: {} because it's part of decoreator syntax", keyword);
				continue;
			}
			String bufLc = buf.toString().toLowerCase();
			int startSearch = 0;
			int foundIdx = -1;
			while ((foundIdx = bufLc.indexOf(keyword, startSearch)) >= 0) {
				if (foundIdx > prefixLength) {
					String kwWithPref = bufLc.substring(foundIdx - prefixLength, foundIdx);
					if (prefix.equals(kwWithPref)) {
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
		LOG.trace("Decorated {} to {}", toDecorate, buf);
		return buf.toString();
	}

	public static interface StringDecorator {
		String decorate(String in);

		String prefix();

		String postfix();
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

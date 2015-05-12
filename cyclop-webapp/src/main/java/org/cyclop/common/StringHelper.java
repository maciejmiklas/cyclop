/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.cyclop.common;

import static java.util.Comparator.comparingInt;

import java.net.InetAddress;
import java.net.UnknownHostException;
import java.util.Arrays;
import java.util.Comparator;
import java.util.Optional;
import java.util.Set;
import java.util.TreeSet;

import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

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

    private static Set<String> sortBySize(String... strArray) {
	Comparator<String> stringComp = String::compareTo;
	Set<String> sorted = new TreeSet<>(comparingInt(String::length).reversed().thenComparing(stringComp));
	sorted.addAll(Arrays.asList(strArray));
	return sorted;
    }

    public static Optional<InetAddress> toInetAddress(String ip) {
	ip = StringUtils.trimToNull(ip);
	if (ip == null) {
	    return Optional.empty();
	}
	InetAddress addr = null;
	try {
	    addr = InetAddress.getByName(ip);
	}
	catch (UnknownHostException | SecurityException e) {
	    LOG.warn("Cannot convert: " + ip + " to valid IP: " + e.getMessage());
	    LOG.debug(e.getMessage(), e);
	}
	return Optional.ofNullable(addr);
    }

    public static interface StringDecorator {
	String decorate(String in);

	String prefix();

	String postfix();
    }
}

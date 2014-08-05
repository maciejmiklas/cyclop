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

import java.util.Arrays;
import java.util.Optional;

import org.apache.commons.lang.StringUtils;
import org.cyclop.model.CqlKeySpace;
import org.cyclop.model.CqlKeyword;
import org.cyclop.model.CqlQuery;
import org.cyclop.model.CqlTable;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/** @author Maciej Miklas */
public class QueryHelper {

    private final static Logger LOG = LoggerFactory.getLogger(QueryHelper.class);

    public static Optional<CqlKeySpace> extractSpace(CqlQuery query) {
	String cqlLc = query.partLc.replaceAll("[;]", "");
	if (!cqlLc.startsWith("use")) {
	    return Optional.empty();
	}

	String space = cqlLc.substring(3, cqlLc.length()).trim();
	space = StringUtils.trimToNull(space);
	if (space == null) {
	    return Optional.empty();
	}
	CqlKeySpace exspace = new CqlKeySpace(space);
	LOG.debug("Extrancted space {} from {}", exspace, query);
	return Optional.of(exspace);
    }

    public static Optional<CqlTable> extractTableName(CqlKeyword cqlKeyword, CqlQuery query) {
	String cqlLc = query.partLc;
	int kwStart = cqlLc.indexOf(cqlKeyword.valueSp);
	if (kwStart == -1) {
	    return Optional.empty();
	}
	kwStart += cqlKeyword.valueSp.length();

	int end = cqlLc.indexOf(" ", kwStart + 1);
	if (end == -1) {
	    end = cqlLc.length();
	}

	String candidate = cqlLc.substring(kwStart, end);
	candidate = StringUtils.trimToNull(candidate);
	if (candidate == null) {
	    return Optional.empty();
	}

	// check whether we have table with keyspace
	CqlTable result = null;
	if (candidate.contains(".") && !candidate.endsWith(".")) {
	    String[] talStr = candidate.split("[.]");
	    String keyspaceStr = StringUtils.trimToNull(talStr[0]);
	    String tableStr = StringUtils.trimToNull(talStr[1]);
	    if (tableStr == null) {
		result = null;
	    }
	    else {
		if (keyspaceStr == null) {
		    result = new CqlTable(candidate);
		}
		else {
		    result = new CqlTable(keyspaceStr, tableStr);
		}
	    }

	}
	else {
	    result = new CqlTable(candidate);
	}

	LOG.debug("Extracted table {} from {}, {}", result, cqlKeyword, query);
	return Optional.ofNullable(result);
    }

    public static Optional<CqlKeySpace> extractKeyspace(CqlQuery query, CqlKeyword... cqlKeyword) {
	Optional<CqlKeySpace> foundKeyspace = Arrays.asList(cqlKeyword).stream()
		.map(kw -> extractKeyspaceSingle(query, kw)).filter(f -> f.isPresent()).findFirst()
		.orElse(Optional.empty());
	return foundKeyspace;
    }

    private static Optional<CqlKeySpace> extractKeyspaceSingle(CqlQuery query, CqlKeyword cqlKeyword) {
	String cqlLc = query.partLc;
	int kwStart = cqlLc.indexOf(cqlKeyword.valueSp);
	if (kwStart == -1) {
	    return Optional.empty();
	}
	kwStart += cqlKeyword.valueSp.length();

	int end = cqlLc.indexOf(".", kwStart + 1);
	if (end == -1) {
	    return Optional.empty();
	}

	String candidate = cqlLc.substring(kwStart, end);
	candidate = StringUtils.trimToNull(candidate);
	if (candidate == null) {
	    return Optional.empty();
	}

	CqlKeySpace space = new CqlKeySpace(candidate);
	return Optional.of(space);
    }
}

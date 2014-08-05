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
package org.cyclop.service.search;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;

import javax.inject.Inject;

import org.cyclop.model.CqlQuery;
import org.cyclop.model.CqlQueryType;
import org.cyclop.model.FilterResult;
import org.cyclop.test.AbstractTestCase;
import org.junit.Test;

import com.google.common.collect.ImmutableSet;

public class TestSearchService extends AbstractTestCase {

	@Inject
	private SearchService<CqlQuery> service;

	private final static CqlQuery[] QUERIES = {new CqlQuery(CqlQueryType.SELECT,
			"0 select * from cqldemo.mybooks where a=11234"), new CqlQuery(CqlQueryType.SELECT,
			"1 select * from cqldemo.mybooks where a=3322425"), new CqlQuery(CqlQueryType.SELECT,
			"2 select attr,id,val from cqldemo.mybooks where a=3322"), new CqlQuery(CqlQueryType.SELECT,
			"3 select attr,id,val from mybooks where a=332233"), new CqlQuery(CqlQueryType.INSERT,
			"4 insert attr,id,val into mybooks where a=3322441"), new CqlQuery(CqlQueryType.INSERT,
			"5 insert attr into mybooks where rid=11233332"), new CqlQuery(CqlQueryType.INSERT,
			"6 insert attr into testbooks where rid=23423"), new CqlQuery(CqlQueryType.SELECT,
			"7 select attr from testbooks where rid=33223323")};

	private final static ImmutableSet<CqlQuery> QUERY_LIST = prepare();

	private final static FieldAccessorCq FAC = new FieldAccessorCq();

	@Test
	public void testFilter_KeywordsToShort() {
		assertNull(service.filter(QUERY_LIST, FAC, "in", "i", "in  ", "  "));
	}

	@Test
	public void testFilter_MinLength_1() {
		FilterResult<CqlQuery> filtered = service.filter(QUERY_LIST, FAC, "fro");
		validateFiltered(filtered, 5, 0, 1, 2, 3, 7);
	}

	@Test
	public void testFilter_ingleAttr_1() {
		FilterResult<CqlQuery> filtered = service.filter(QUERY_LIST, FAC, "from ");
		validateFiltered(filtered, 5, 0, 1, 2, 3, 7);
	}

	@Test
	public void testFilter_SingleAttr_2() {
		FilterResult<CqlQuery> filtered = service.filter(QUERY_LIST, FAC, "cqldemo");
		validateFiltered(filtered, 3, 0, 1, 2);
	}

	@Test
	public void testFilter_SingleAttr_3() {
		FilterResult<CqlQuery> filtered = service.filter(QUERY_LIST, FAC, "attr");
		validateFiltered(filtered, 6, 2, 3, 4, 5, 6, 7);
	}

	@Test
	public void testFilter_SingleAttr_4() {
		FilterResult<CqlQuery> filtered = service.filter(QUERY_LIST, FAC, "3322");
		validateFiltered(filtered, 5, 1, 2, 3, 4, 7);
	}

	@Test
	public void testFilter_SingleAttr_5() {
		FilterResult<CqlQuery> filtered = service.filter(QUERY_LIST, FAC, "mybooks");
		validateFiltered(filtered, 6, 0, 1, 2, 3, 4, 5);
	}

	@Test
	public void testFilter_SingleAttr_6() {
		FilterResult<CqlQuery> filtered = service.filter(QUERY_LIST, FAC, "332");
		validateFiltered(filtered, 6, 1, 2, 3, 4, 5, 7);
	}

	@Test
	public void testFilter_TwoAttrs_1() {
		FilterResult<CqlQuery> filtered = service.filter(QUERY_LIST, FAC, "332", "from");
		validateFiltered(filtered, 7, 1, 2, 3, 7, 0, 4, 5);
	}

	@Test
	public void testFilter_TwoAttrs_2() {
		FilterResult<CqlQuery> filtered = service.filter(QUERY_LIST, FAC, "332", "insert");
		validateFiltered(filtered, 7, 4, 5, 1, 2, 3, 6, 7);
	}

	@Test
	public void testFilter_ThreeAttrs() {
		FilterResult<CqlQuery> filtered = service.filter(QUERY_LIST, FAC, "332", "from", "insert");
		validateFiltered(filtered, 8, 1, 2, 3, 4, 5, 7, 0, 6);
	}

	@Test
	public void testFilter_ThreeAttrsCaseSensitive() {
		FilterResult<CqlQuery> filtered = service.filter(QUERY_LIST, FAC, "332", "fRom", "INSERT");
		validateFiltered(filtered, 8, 1, 2, 3, 4, 5, 7, 0, 6);
	}

	@Test
	public void testFilter_FourAttrs_1() {
		FilterResult<CqlQuery> filtered = service.filter(QUERY_LIST, FAC, "332", "from", "insert", "select");
		validateFiltered(filtered, 8, 1, 2, 3, 7, 0, 4, 5, 6);
	}

	@Test
	public void testFilter_FourAttrs_2() {
		FilterResult<CqlQuery> filtered = service.filter(QUERY_LIST, FAC, "332", "from", "insert", "into");
		validateFiltered(filtered, 8, 4, 5, 1, 2, 3, 6, 7, 0);
	}

	@Test
	public void testFilter_FourAttrs_3() {
		FilterResult<CqlQuery> filtered = service.filter(QUERY_LIST, FAC, "3322", "from", "insert", "into");
		validateFiltered(filtered, 8, 4, 1, 2, 3, 5, 6, 7, 0);
	}

	private void validateFiltered(FilterResult<CqlQuery> filterRes, int size, int... queries) {
		assertEquals(size, filterRes.result.size());
		assertNotNull(filterRes.normalizedKeywords);
		assertFalse(filterRes.normalizedKeywords.isEmpty());
		int listIdx = 0;
		for (int resIdx : queries) {
			CqlQuery query = filterRes.result.get(listIdx++);
			assertNotNull("Not found: " + QUERIES[resIdx], query);
		}

		assertEquals(size, queries.length);
	}

	private static ImmutableSet<CqlQuery> prepare() {
		ImmutableSet.Builder<CqlQuery> res = ImmutableSet.builder();
		for (CqlQuery query : QUERIES) {
			res.add(query);
		}
		return res.build();
	}

	private final static class FieldAccessorCq implements FieldAccessor<CqlQuery> {

		@Override
		public String getText(CqlQuery obj) {
			return obj.partLc;
		}

	}
}

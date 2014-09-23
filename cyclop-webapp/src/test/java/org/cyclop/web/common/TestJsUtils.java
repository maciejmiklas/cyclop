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
package org.cyclop.web.common;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;

import org.cyclop.model.CqlColumnName;
import org.junit.Test;

import com.google.common.collect.ImmutableSet;

/** @author Maciej Miklas */
public class TestJsUtils {

	@Test(expected = IllegalArgumentException.class)
	public void testEscape_Null() {
		assertNull(JsUtils.escape(null));
	}

	@Test
	public void testEscape() {
		assertEquals("\"test value to escape .... ;)'\"", JsUtils.escape("test value to escape .... ;)'"));
	}

	@Test(expected = IllegalArgumentException.class)
	public void testEscapeArray_Null() {
		assertNull(JsUtils.escapeArray(null));
	}

	@Test(expected = IllegalArgumentException.class)
	public void testEscapeStrArray_Null() {
		assertNull(JsUtils.escapeStrArray(null));
	}

	@Test
	public void testEscapeArray() {
		assertEquals("[\"col 1\",\"col 1231\",\"2col 41\"]", JsUtils.escapeArray(ImmutableSet.of(new CqlColumnName(
				"col 1"), new CqlColumnName("col 1"), new CqlColumnName("col 1231"), new CqlColumnName("2col 41"))));
	}

	@Test
	public void testEscapeStrArray() {
		assertEquals("[\"123123\",\"param two\"]", JsUtils.escapeStrArray(ImmutableSet.of("123123", "param two")));
	}

}

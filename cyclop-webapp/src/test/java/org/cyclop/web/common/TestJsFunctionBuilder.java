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

import org.cyclop.model.CqlColumnName;
import org.junit.Test;

import com.google.common.collect.ImmutableSet;

/** @author Maciej Miklas */
public class TestJsFunctionBuilder {

	@Test
	public void testNoParameters() {
		assertEquals("refreshPage()", JsFunctionBuilder.function("refreshPage").build());
	}

	@Test
	public void testMixedParameters() {
		assertEquals(
				"refreshPage([\"123123\",\"param two\"],\"param 1\",\"param 2\",[\"col 1\",\"col 1231\",\"2col 41\"],\"param 4\")",
				JsFunctionBuilder.function("refreshPage").arrayStr(ImmutableSet.of("123123", "param two"))
						.param("param 1").param("param 2").array(ImmutableSet
						.of(new CqlColumnName("col 1"), new CqlColumnName("col 1"), new CqlColumnName("col 1231"),
								new CqlColumnName("2col 41"))).param("param 4").build());
	}

	@Test
	public void tesSimpleParameters() {
		assertEquals("refreshPage(\"param 1\",\"param 2\")",
				JsFunctionBuilder.function("refreshPage").param("param 1").param("param 2").build());
	}
}

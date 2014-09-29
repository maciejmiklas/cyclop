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

import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableSet;
import org.junit.Test;

import java.util.Arrays;
import java.util.List;

import static org.cyclop.common.Gullectors.toImmutableList;
import static org.cyclop.common.Gullectors.toImmutableSet;
import static org.cyclop.common.Gullectors.toNaturalImmutableSortedSet;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

/** @author Maciej Miklas */
public class TestGullectors {

	@Test
	public void testToNaturalImmutableSortedSet_Sort() {
		List<String> vals = Arrays.asList("charlie", "bravo", "delta", "india", "echo", "alpha");
		ImmutableSet<String> res = vals.stream().collect(toNaturalImmutableSortedSet());
		assertEquals("[alpha, bravo, charlie, delta, echo, india]", res.toString());
	}

	@Test
	public void testToNaturalImmutableSortedSet_Empty() {
		List<String> vals = Arrays.asList();
		ImmutableSet<String> res = vals.stream().collect(toNaturalImmutableSortedSet());
		assertTrue(res.isEmpty());
	}

	@Test
	public void testToImmutableSet_Sort() {
		List<Integer> vals = Arrays.asList(5, 1, 22, 4, 8, 9, 3, 3, 989);
		ImmutableSet<Integer> res = vals.stream().collect(toImmutableSet());
		assertEquals("[5, 1, 22, 4, 8, 9, 3, 989]", res.toString());

	}

	@Test
	public void testToImmutableSet_Empty() {
		List<String> vals = Arrays.asList();
		ImmutableSet<String> res = vals.stream().collect(toImmutableSet());
		assertTrue(res.isEmpty());
	}


	@Test
	public void testToImmutableList_Sort() {
		List<Integer> vals = Arrays.asList(1, 3, 4, 5, 8, 9, 3, 989);
		ImmutableList<Integer> res = vals.stream().collect(toImmutableList());
		assertEquals("[1, 3, 4, 5, 8, 9, 3, 989]", res.toString());
	}
}

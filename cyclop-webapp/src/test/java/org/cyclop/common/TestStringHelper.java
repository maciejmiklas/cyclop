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

import static org.junit.Assert.assertEquals;

import org.cyclop.common.StringHelper.StringDecorator;
import org.junit.Test;

public class TestStringHelper {

    private final static TestDecorator TD = new TestDecorator();

    @Test
    public void testDecorate_ContainsDecoratedText() {
	String decorated = StringHelper
		.decorate(
			"select +++ from +++cqldemo----.mybooks where id=1122 and some value is not ++++++++ fromstone",
			TD,
			"from",
			"fromsto",
			"112",
			"demo",
			"not",
			"+++");

	assertEquals(
		"select +++ +++++from-- +++cql+++++demo------.mybooks where id=+++++112--2 and some value is +++++not-- ++++++++ +++++fromsto--ne",
		decorated);
    }

    @Test
    public void testDecorate_ContainitgWords_ShorterFirst() {
	String decorated = StringHelper.decorate(
		"select * from cqldemo.mybooks where id=1122 and some value is not .... fromstone",
		TD,
		"from",
		"fromsto",
		"112",
		"demo",
		"not");

	assertEquals(
		"select * +++++from-- cql+++++demo--.mybooks where id=+++++112--2 and some value is +++++not-- .... +++++fromsto--ne",
		decorated);
    }

    @Test
    public void testDecorate_ContainitgWords_LongerFirst() {
	String decorated = StringHelper.decorate(
		"select * from cqldemo.mybooks where id=1122 and some value is not .... fromstone",
		TD,
		"fromsto",
		"from",
		"112",
		"demo",
		"not");

	assertEquals(
		"select * +++++from-- cql+++++demo--.mybooks where id=+++++112--2 and some value is +++++not-- .... +++++fromsto--ne",
		decorated);
    }

    @Test
    public void testDecorate_ContainitgWords_FewLonger() {
	String decorated = StringHelper
		.decorate(
			"select fromstore,demo45,demo123,demo1,demo342,demos231,demos12 from cqldemo.mybooks where id=1122 and some value is not .... fromstone",
			TD,
			"fromsto",
			"from",
			"112",
			"demo",
			"demos",
			"demo123",
			"not");

	assertEquals(
		"select +++++fromsto--re,+++++demo--45,+++++demo123--,+++++demo--1,+++++demo--342,+++++demos--231,+++++demos--12 +++++from-- cql+++++demo--.mybooks where id=+++++112--2 and some value is +++++not-- .... +++++fromsto--ne",
		decorated);
    }

    @Test
    public void testDecorate_LowerCase() {
	String decorated = StringHelper.decorate(
		"select * from cqldemo.mybooks where id=1122 and some value is not .... from",
		TD,
		"from",
		"112",
		"demo",
		"not");

	assertEquals(
		"select * +++++from-- cql+++++demo--.mybooks where id=+++++112--2 and some value is +++++not-- .... +++++from--",
		decorated);
    }

    @Test
    public void testDecorate_UpperCase() {
	String decorated = StringHelper.decorate(
		"SELECT * FROM CQLDEMO.MYBOOKS WHERE ID=1122 AND SOME VALUE IS NOT .... FROM",
		TD,
		"from",
		"112",
		"demo",
		"not");

	assertEquals(
		"SELECT * +++++FROM-- CQL+++++DEMO--.MYBOOKS WHERE ID=+++++112--2 AND SOME VALUE IS +++++NOT-- .... +++++FROM--",
		decorated);
    }

    @Test
    public void testDecorate_Mixed() {
	String decorated = StringHelper.decorate(
		"SeLEcT * FRoM CqLDEmO.MYBOoKS WhERe ID=1122 AND SOME VALUE IS not .... FROM",
		TD,
		"from",
		"112",
		"demo",
		"not");

	assertEquals(
		"SeLEcT * +++++FRoM-- CqL+++++DEmO--.MYBOoKS WhERe ID=+++++112--2 AND SOME VALUE IS +++++not-- .... +++++FROM--",
		decorated);
    }

    @Test
    public void testDecorate_Start() {
	String decorated = StringHelper.decorate(
		"SeLEcT * FRoM CqLDEmO.MYBOoKS WhERe ID=1122 AND SOME VALUE IS not .... FROM",
		TD,
		"select",
		"112",
		"demo",
		"not");

	assertEquals(
		"+++++SeLEcT-- * FRoM CqL+++++DEmO--.MYBOoKS WhERe ID=+++++112--2 AND SOME VALUE IS +++++not-- .... FROM",
		decorated);
    }

    private final static class TestDecorator implements StringDecorator {

	@Override
	public String decorate(String in) {
	    return prefix() + in + postfix();
	}

	@Override
	public String prefix() {
	    return "+++++";
	}

	@Override
	public String postfix() {
	    return "--";
	}
    }
}

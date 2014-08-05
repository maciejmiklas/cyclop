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
package org.cyclop.service.common;

import javax.inject.Inject;

import org.cyclop.model.exception.BeanValidationException;
import org.cyclop.service.common.CookieStorage.CookieName;
import org.cyclop.test.AbstractTestCase;
import org.junit.Test;

/** @author Maciej Miklas */
public class TestCookieStorage extends AbstractTestCase {

	@Inject
	private CookieStorage storage;

	@Test(expected = BeanValidationException.class)
	public void testReadCookie_Validation() {
		storage.readCookie(null);
	}

	@Test(expected = BeanValidationException.class)
	public void testReadCookieAsJson_Validation() {
		storage.readCookieAsJson(null, null);
	}

	@Test(expected = BeanValidationException.class)
	public void testStoreCookie_Validation() {
		storage.storeCookie(null, null);
	}

	@Test(expected = BeanValidationException.class)
	public void testStoreCookieAsJson_Validation() {
		storage.storeCookieAsJson(CookieName.cyclop_prefs, null);
	}

}

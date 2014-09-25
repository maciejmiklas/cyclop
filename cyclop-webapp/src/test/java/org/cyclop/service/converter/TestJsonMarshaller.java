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
package org.cyclop.service.converter;

import static org.junit.Assert.assertEquals;

import javax.inject.Inject;

import org.cyclop.model.UserPreferences;
import org.cyclop.test.AbstractTestCase;
import org.junit.Test;

/** @author Maciej Miklas */
public class TestJsonMarshaller extends AbstractTestCase {

	@Inject
	private JsonMarshaller marshaller;

	@Test
	public void testMarshal() {
		UserPreferences up = new UserPreferences();
		up.setShowCqlCompletionHint(false);
		String res = marshaller.marshal(up);
		assertEquals(
				"{\"e_hi\":\"0\",\"e_he\":\"1\",\"e_ro\":0,\"i_hi\":\"1\",\"i_ce\":\"0\",\"i_pa\":\"0\",\"p_ei\":5,\"p_hi\":50,\"p_ii\":100}",
				res);
	}

	@Test
	public void testUnmarshal() {
		UserPreferences res = marshaller.unmarshal(UserPreferences.class, "{\"e_hi\":\"0\",\"e_he\":\"1\"}");

		UserPreferences up = new UserPreferences();
		up.setShowCqlCompletionHint(false);
		assertEquals(up, res);
	}

	@Test
	public void testUnmarshalDefaults() {
		UserPreferences res = marshaller.unmarshal(UserPreferences.class, "{\"e_hi\":\"0\"}");

		UserPreferences up = new UserPreferences();
		up.setShowCqlCompletionHint(false);
		assertEquals(up, res);
	}

}

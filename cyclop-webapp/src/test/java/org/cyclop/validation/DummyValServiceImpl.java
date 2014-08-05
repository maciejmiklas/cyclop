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
package org.cyclop.validation;

import static org.junit.Assert.assertNotNull;

import javax.inject.Named;

import org.cyclop.model.CqlKeySpace;
import org.cyclop.model.CqlTable;
import org.cyclop.model.exception.ServiceException;

@EnableValidation
@Named
public class DummyValServiceImpl implements DummyValService {

    @Override
    public String singleStringParam(String a) {
	assertNotNull(a);
	return a;
    }

    @Override
    public CqlKeySpace mixedParams(CqlKeySpace space, CqlTable table, String param) {
	assertNotNull(param);
	return space;
    }

    @Override
    public void singleParam(CqlTable table) {

    }

    @Override
    public void singleParamThrows(CqlTable table) throws Exception {
	if (table.partLc.equals("throw")) {
	    throw new ServiceException("ex abc");
	}
    }

    @Override
    public CqlKeySpace returnOptional(CqlKeySpace space) {
	return space;
    }

    @Override
    public CqlKeySpace novalidation(CqlKeySpace space) {
	return space;
    }

}

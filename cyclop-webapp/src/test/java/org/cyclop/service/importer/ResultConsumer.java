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
package org.cyclop.service.importer;

import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import net.jcip.annotations.ThreadSafe;

import org.cyclop.model.CqlQuery;
import org.cyclop.model.exception.QueryException;

/** @author Maciej Miklas */
@ThreadSafe
class ResultConsumer implements ResultWriter {

	public List<CqlQuery> success = new ArrayList<>();

	public Map<CqlQuery, Exception> error = new HashMap<>();

	@Override
	public synchronized void success(CqlQuery query, long runtime) {
		assertTrue(runtime >= 0);
		success.add(query);
	}

	@Override
	public synchronized void error(CqlQuery query, QueryException ex, long runtime) {
		assertTrue(runtime >= 0);
		error.put(query, ex);
	}

	@Override
	public synchronized void unknownError(CqlQuery query, Exception error, long runtime) {
		fail();
	}

	public synchronized int size() {
		return success.size() + error.size();
	}

	@Override
	public synchronized String toString() {
		return "ResultConsumer [success=" + success + ", error=" + error + "]";
	}


}

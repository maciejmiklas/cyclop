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
package org.cyclop.service.importer.impl;

import java.util.Scanner;

import javax.inject.Inject;
import javax.inject.Named;

import org.apache.commons.lang.StringUtils;
import org.cyclop.model.CqlQuery;
import org.cyclop.model.CqlQueryType;
import org.cyclop.model.exception.QueryException;
import org.cyclop.service.cassandra.QueryService;
import org.cyclop.service.importer.QueryImporter;
import org.cyclop.service.importer.ResultWriter;
import org.cyclop.service.importer.model.ImportConfig;
import org.cyclop.validation.EnableValidation;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/** @author Maciej Miklas */
@Named(QueryImporter.IMPL_SERIAL)
@EnableValidation
public class SerialQueryImporter extends AbstractImporter {

	private final static Logger LOG = LoggerFactory.getLogger(SerialQueryImporter.class);

	@Inject
	protected QueryService queryService;

	@Override
	void execImport(Scanner scanner, ResultWriter resultWriter, StatsCollector status, ImportConfig config) {
		while (scanner.hasNext()) {
			String nextStr = StringUtils.trimToNull(scanner.next());
			if (nextStr == null) {
				continue;
			}
			CqlQuery query = new CqlQuery(CqlQueryType.UNKNOWN, nextStr);
			long startTime = System.currentTimeMillis();
			try {
				LOG.debug("Executing: {}", query);
				queryService.executeSimple(query, config.isUpdateHistory());
				resultWriter.success(query, System.currentTimeMillis() - startTime);
				status.success.getAndIncrement();
			} catch (QueryException e) {
				status.error.getAndIncrement();
				LOG.debug(e.getMessage());
				LOG.trace(e.getMessage(), e);
				resultWriter.error(query, e, System.currentTimeMillis() - startTime);

				if (!config.isContinueWithErrors()) {
					LOG.debug("Breaking import due to an error");
					break;
				}
			}
		}
	}
}

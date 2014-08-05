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

import java.io.InputStream;
import java.util.Scanner;

import javax.inject.Inject;

import org.apache.commons.lang3.time.StopWatch;
import org.cyclop.common.AppConfig;
import org.cyclop.service.importer.QueryImporter;
import org.cyclop.service.importer.ResultWriter;
import org.cyclop.service.importer.model.ImportConfig;
import org.cyclop.service.importer.model.ImportStats;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/** @author Maciej Miklas */
abstract class AbstractImporter implements QueryImporter {
    private final static Logger LOG = LoggerFactory.getLogger(AbstractImporter.class);

    @Inject
    protected AppConfig conf;

    @Override
    public final ImportStats importScript(InputStream input, ResultWriter resultWriter, ImportConfig config) {
	LOG.debug("Starting query import");
	StopWatch timer = new StopWatch();
	timer.start();

	StatsCollector status = new StatsCollector();
	Scanner scanner = new Scanner(input, conf.queryImport.encoding);
	scanner.useDelimiter(conf.queryImport.listSeparatorRegEx);

	LOG.debug("Executing import");
	execImport(scanner, resultWriter, status, config);

	timer.stop();
	ImportStats stats = new ImportStats(timer, status.success.get(), status.error.get());

	LOG.debug("Import done: {}", stats);
	return stats;
    }

    abstract void execImport(
	    Scanner scanner,
	    ResultWriter resultWriter,
	    StatsCollector status,
	    ImportConfig config);

}

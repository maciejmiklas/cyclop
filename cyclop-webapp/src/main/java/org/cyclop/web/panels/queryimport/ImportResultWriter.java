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
package org.cyclop.web.panels.queryimport;

import java.util.UUID;

import net.jcip.annotations.ThreadSafe;

import org.cyclop.model.CqlQuery;
import org.cyclop.model.exception.QueryException;
import org.cyclop.service.importer.ResultWriter;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.common.collect.ImmutableList;

/** @author Maciej Miklas */
@ThreadSafe
final class ImportResultWriter implements ResultWriter {

    private final static Logger LOG = LoggerFactory.getLogger(QueryImportPanel.class);

    private final ImmutableList.Builder<ImportResult> resultBuild = ImmutableList.builder();

    public synchronized ImmutableList<ImportResult> getResult() {
	return resultBuild.build();
    }

    @Override
    public synchronized void success(CqlQuery query, long runtime) {
	resultBuild.add(new ImportResult(query, runtime));
    }

    @Override
    public synchronized void error(CqlQuery query, QueryException error, long runtime) {
	resultBuild.add(new ImportResult(query, error.getMessage(), runtime));
    }

    @Override
    public synchronized void unknownError(CqlQuery query, Exception ex, long runtime) {
	String errorRef = UUID.randomUUID().toString();
	String errorMsg = "Unhandled exception, Error ID: " + errorRef;
	LOG.error(errorMsg, ex);
	resultBuild.add(new ImportResult(query, errorMsg, runtime));
    }

}

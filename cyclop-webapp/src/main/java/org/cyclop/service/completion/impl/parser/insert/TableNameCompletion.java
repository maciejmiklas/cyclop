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
package org.cyclop.service.completion.impl.parser.insert;

import javax.inject.Inject;
import javax.inject.Named;

import org.cyclop.model.CqlCompletion;
import org.cyclop.model.CqlKeyword;
import org.cyclop.model.CqlQuery;
import org.cyclop.service.completion.impl.parser.CompletionHelper;
import org.cyclop.service.completion.impl.parser.MarkerBasedCompletion;

import com.google.common.base.Objects;

/** @author Maciej Miklas */
@Named("insert.TableNameCompletion")
class TableNameCompletion extends MarkerBasedCompletion {

    @Inject
    private CompletionHelper completionHelper;

    public TableNameCompletion() {
	super(CqlKeyword.Def.INSERT_INTO.value);
    }

    @Override
    public CqlCompletion getCompletion(CqlQuery query) {
	CqlCompletion.Builder completion = completionHelper.computeTableNameCompletion(
		query,
		CqlKeyword.Def.INSERT_INTO.value);
	return completion.build();
    }

    @Override
    public String toString() {
	return Objects.toStringHelper(this).toString();
    }

}

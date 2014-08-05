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
package org.cyclop.web.common;

import static org.cyclop.web.common.JsUtils.escape;
import static org.cyclop.web.common.JsUtils.escapeArray;
import static org.cyclop.web.common.JsUtils.escapeStrArray;

import java.util.Collection;

import org.apache.commons.lang.Validate;
import org.cyclop.model.DisplaySupport;
import org.cyclop.web.common.JsUtils.ObjPrinter;

public final class JsFunctionBuilder {

    private final StringBuilder buf;

    private boolean firstPar = true;

    private boolean build = false;

    public static JsFunctionBuilder function(String functionName) {
	Validate.notNull(functionName, "Null functionName");
	return new JsFunctionBuilder(functionName);
    }

    private JsFunctionBuilder(String functionName) {
	Validate.notNull(functionName, "Null functionName");
	buf = new StringBuilder(functionName);
	buf.append("(");
    }

    public JsFunctionBuilder param(String param) {
	Validate.notNull(param, "Null param");
	sep();
	buf.append(escape(param));
	return this;
    }

    public JsFunctionBuilder array(Collection<? extends DisplaySupport> col) {
	Validate.notNull(col, "Null col");
	sep();
	buf.append(escapeArray(col));
	return this;
    }

    public JsFunctionBuilder arrayStr(Collection<String> col) {
	Validate.notNull(col, "Null col");
	sep();
	buf.append(escapeStrArray(col));
	return this;
    }

    public <T> JsFunctionBuilder array(Collection<T> col, ObjPrinter<T> printer) {
	Validate.notNull(col, "Null col");
	Validate.notNull(printer, "Null printer");
	sep();
	buf.append(escapeArray(col, printer));
	return this;
    }

    public String build() {
	if (build) {
	    throw new IllegalArgumentException("JsFunctionBuilder.build() can be called only once");
	}
	build = true;
	buf.append(")");
	return buf.toString();
    }

    private void sep() {
	if (firstPar) {
	    firstPar = false;
	    return;
	}
	buf.append(",");
    }

}

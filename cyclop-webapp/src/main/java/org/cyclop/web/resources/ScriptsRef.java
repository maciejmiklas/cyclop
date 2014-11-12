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
package org.cyclop.web.resources;

import org.apache.wicket.request.resource.JavaScriptResourceReference;

/** @author Maciej Miklas */
public final class ScriptsRef {

	public static final JavaScriptResourceReference JQUERY_TOOLS = new JavaScriptResourceReference(ScriptsRef.class,
			"js/jquery.a-tools.js");

	public static final JavaScriptResourceReference COMMON = new JavaScriptResourceReference(ScriptsRef.class,
			"js/common.js");

	public static final JavaScriptResourceReference JQUERY_BROWSER = new JavaScriptResourceReference(ScriptsRef.class,
			"js/jquery.browser.js");

	public static final JavaScriptResourceReference RWD_TABLE = new JavaScriptResourceReference(ScriptsRef.class,
			"rwd-table/js/rwd-table.js");

	public static final JavaScriptResourceReference BOOTSTRAP = new JavaScriptResourceReference(ScriptsRef.class,
			"bootstrap/js/bootstrap.js");

	public static final JavaScriptResourceReference JQUERY_UI = new JavaScriptResourceReference(ScriptsRef.class,
			"jqueryui/js/jquery-ui.js");
}

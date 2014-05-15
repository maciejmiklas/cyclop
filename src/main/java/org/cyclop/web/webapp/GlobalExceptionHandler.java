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
package org.cyclop.web.webapp;

import org.apache.wicket.core.request.handler.PageProvider;
import org.apache.wicket.core.request.handler.RenderPageRequestHandler;
import org.apache.wicket.request.IRequestHandler;
import org.apache.wicket.request.cycle.AbstractRequestCycleListener;
import org.apache.wicket.request.cycle.RequestCycle;
import org.cyclop.model.exception.AuthenticationRequiredException;
import org.cyclop.web.pages.authenticate.AuthenticationPage;

/** @author Maciej Miklas */
public class GlobalExceptionHandler extends AbstractRequestCycleListener {

	@Override
	public IRequestHandler onException(RequestCycle cycle, Exception ex) {
		if (ex instanceof AuthenticationRequiredException) {
			return new RenderPageRequestHandler(new PageProvider(AuthenticationPage.class));
		}
		return super.onException(cycle, ex);
	}

}

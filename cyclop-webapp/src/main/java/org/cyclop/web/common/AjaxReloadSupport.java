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

/** @author Maciej Miklas */
public interface AjaxReloadSupport {

    /**
     * @return ajax link that can be used to trigger reload of this component
     *         (or part of it).
     */
    String getReloadCallbackUrl();

    /**
     * @return CSS class name used to find ajax-reloadable-content of this
     *         component. Content of this tag will be removed in browser, when
     *         user switches to another tab. Main reason to do that is
     *         performance improvements - we try to keep dom tree as small as
     *         possible
     */
    String getRemovableContentCssRef();

}

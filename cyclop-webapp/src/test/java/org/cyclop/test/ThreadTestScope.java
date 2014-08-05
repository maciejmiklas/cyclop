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
package org.cyclop.test;

import java.util.HashMap;
import java.util.Map;

import javax.inject.Named;

import org.springframework.beans.factory.ObjectFactory;
import org.springframework.context.support.SimpleThreadScope;

/** @author Maciej Miklas */
@Named
public class ThreadTestScope extends SimpleThreadScope {

    private Map<String, Object> map = new HashMap<>();

    private boolean singleThread = false;

    public synchronized void setSingleThread(boolean singleThread) {
	this.singleThread = singleThread;
    }

    @Override
    @SuppressWarnings("rawtypes")
    public synchronized Object get(String name, ObjectFactory objectFactory) {
	if (singleThread) {
	    Object object = map.get(name);
	    if (object == null) {
		object = objectFactory.getObject();
		map.put(name, object);
	    }
	    return object;
	}
	else {
	    return super.get(name, objectFactory);
	}
    }

    @Override
    public synchronized Object remove(String s) {
	if (singleThread) {
	    return map.remove(s);
	}
	else {
	    return super.remove(s);
	}
    }

    @Override
    public void registerDestructionCallback(String s, Runnable runnable) {
    }

    @Override
    public Object resolveContextualObject(String s) {
	return null;
    }

    @Override
    public String getConversationId() {
	return Thread.currentThread().getName();
    }

}

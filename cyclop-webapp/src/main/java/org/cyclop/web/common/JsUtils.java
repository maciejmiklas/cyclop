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

import org.cyclop.model.DisplaySupport;

import java.util.Collection;
import java.util.Iterator;

/** @author Maciej Miklas */
public final class JsUtils {

	public static String escape(String param) {
		if (param == null) {
			throw new IllegalArgumentException("Null param");
		}
		return "\"" + param + "\"";
	}

	public static <T extends DisplaySupport> String escapeArray(Collection<T> col) {
		if (col == null) {
			throw new IllegalArgumentException("Null param");
		}
		return escapeArray(col, new DisplayObjPrinter<T>());
	}

	public static String escapeStrArray(Collection<String> col) {
		return escapeArray(col, new StringObjPrinter());
	}

	public static <T> String escapeArray(Collection<T> col, ObjPrinter<T> printer) {
		if (col == null || printer == null) {
			throw new IllegalArgumentException("Null param");
		}

		StringBuilder buf = new StringBuilder();
		buf.append("[");

		Iterator<T> colIt = col.iterator();
		while (colIt.hasNext()) {
			T next = colIt.next();
			String nextStr = printer.print(next);
			String nextEsc = escape(nextStr);
			buf.append(nextEsc);
			if (colIt.hasNext()) {
				buf.append(",");
			}
		}
		buf.append("]");
		return buf.toString();
	}

	public static interface ObjPrinter<T> {
		String print(T obj);
	}

	private final static class DisplayObjPrinter<T extends DisplaySupport> implements ObjPrinter<T> {

		@Override
		public String print(T obj) {
			return obj.toDisplayString();
		}
	}

	;

	private final static class StringObjPrinter implements ObjPrinter<String> {

		@Override
		public String print(String obj) {
			return obj.toString();
		}
	}

	;
}

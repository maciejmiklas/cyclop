package org.cyclop.web.common;

import org.cyclop.model.DisplaySupport;

import java.util.Collection;
import java.util.Iterator;

// TODO add tests

/** @author Maciej Miklas */
public class JsUtils {

	public static String escape(String param) {
		if (param == null) {
			return null;
		}
		return "'" + param + "'";
	}

	public static <T extends DisplaySupport> String escapeArray(Collection<T> col) {
		return escapeArray(col, new DisplayObjPrinter<T>());
	}

	public static String escapeStrArray(Collection<String> col) {
		return escapeArray(col, new StringObjPrinter());
	}

	public static <T> String escapeArray(Collection<T> col, ObjPrinter<T> printer) {
		if (col == null) {
			return null;
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

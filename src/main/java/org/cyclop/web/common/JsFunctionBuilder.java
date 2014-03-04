package org.cyclop.web.common;

import org.cyclop.model.DisplaySupport;
import org.cyclop.web.common.JsUtils.ObjPrinter;

import java.util.Collection;

import static org.cyclop.web.common.JsUtils.escape;
import static org.cyclop.web.common.JsUtils.escapeArray;
import static org.cyclop.web.common.JsUtils.escapeStrArray;

public final class JsFunctionBuilder {

	private final StringBuilder buf;

	private boolean firstPar = true;

	private boolean build = false;

	public static JsFunctionBuilder function(String functionName) {
		if (functionName == null) {
			throw new IllegalArgumentException("Null functionName");
		}
		return new JsFunctionBuilder(functionName);
	}

	private JsFunctionBuilder(String functionName) {
		if (functionName == null) {
			throw new IllegalArgumentException("Null functionName");
		}
		buf = new StringBuilder(functionName);
		buf.append("(");
	}

	public JsFunctionBuilder param(String param) {
		if (param == null) {
			throw new IllegalArgumentException("Null param");
		}
		sep();
		buf.append(escape(param));
		return this;
	}

	public JsFunctionBuilder array(Collection<? extends DisplaySupport> col) {
		if (col == null) {
			throw new IllegalArgumentException("Null col");
		}
		sep();
		buf.append(escapeArray(col));
		return this;
	}

	public JsFunctionBuilder arrayStr(Collection<String> col) {
		if (col == null) {
			throw new IllegalArgumentException("col");
		}
		sep();
		buf.append(escapeStrArray(col));
		return this;
	}

	public <T> JsFunctionBuilder array(Collection<T> col, ObjPrinter<T> printer) {
		if (col == null || printer == null) {
			throw new IllegalArgumentException("Null col or printer");
		}
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

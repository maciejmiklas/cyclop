package org.cyclop.web.common;

import org.cyclop.model.DisplaySupport;
import org.cyclop.web.common.JsUtils.ObjPrinter;

import java.util.Collection;

import static org.cyclop.web.common.JsUtils.escape;
import static org.cyclop.web.common.JsUtils.escapeArray;
import static org.cyclop.web.common.JsUtils.escapeStrArray;

// TODO tests
public class JsFunctionBuilder {

	private final StringBuilder buf;

	private boolean firstPar = true;

	private boolean build = false;

	public static JsFunctionBuilder function(String functionName) {
		return new JsFunctionBuilder(functionName);
	}

	private JsFunctionBuilder(String functionName) {
		buf = new StringBuilder(functionName);
		buf.append("(");
	}

	public JsFunctionBuilder param(String param) {
		sep();
		buf.append(escape(param));
		return this;
	}

	public JsFunctionBuilder array(Collection<? extends DisplaySupport> col) {
		sep();
		buf.append(escapeArray(col));
		return this;
	}

	public JsFunctionBuilder arrayStr(Collection<String> col) {
		sep();
		buf.append(escapeStrArray(col));
		return this;
	}

	public <T> JsFunctionBuilder array(Collection<T> col, ObjPrinter<T> printer) {
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

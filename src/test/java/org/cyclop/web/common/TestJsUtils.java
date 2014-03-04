package org.cyclop.web.common;

import com.google.common.collect.ImmutableSet;
import org.cyclop.model.CqlColumnName;
import org.junit.Test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;

/** @author Maciej Miklas */
public class TestJsUtils {

	@Test(expected = IllegalArgumentException.class)
	public void testEscape_Null() {
		assertNull(JsUtils.escape(null));
	}

	@Test
	public void testEscape() {
		assertEquals("'test value to escape .... ;)'", JsUtils.escape("test value to escape .... ;)"));
	}

	@Test(expected = IllegalArgumentException.class)
	public void testEscapeArray_Null() {
		assertNull(JsUtils.escapeArray(null));
	}

	@Test(expected = IllegalArgumentException.class)
	public void testEscapeStrArray_Null() {
		assertNull(JsUtils.escapeStrArray(null));
	}

	@Test
	public void testEscapeArray() {
		assertEquals("['col 1','col 1231','2col 41']", JsUtils.escapeArray(ImmutableSet
				.of(new CqlColumnName("col 1"), new CqlColumnName("col 1"), new CqlColumnName("col 1231"),
						new CqlColumnName("2col 41"))));
	}

	@Test
	public void testEscapeStrArray() {
		assertEquals("['123123','param two']", JsUtils.escapeStrArray(ImmutableSet.of("123123", "param two")));
	}

}

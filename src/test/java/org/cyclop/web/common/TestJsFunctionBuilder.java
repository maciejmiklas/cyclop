package org.cyclop.web.common;

import com.google.common.collect.ImmutableSet;
import org.cyclop.model.CqlColumnName;
import org.junit.Test;

import static org.junit.Assert.assertEquals;

/** @author Maciej Miklas */
public class TestJsFunctionBuilder {

	@Test
	public void testNoParameters() {
		assertEquals("refreshPage()", JsFunctionBuilder.function("refreshPage").build());
	}

	@Test
	public void testMixedParameters() {
		assertEquals("refreshPage(['123123','param two'],'param 1','param 2',['col 1','col 1231','2col 41'],'param 4')",
				JsFunctionBuilder.function("refreshPage").arrayStr(ImmutableSet.of("123123", "param two"))
						.param("param 1").param("param 2").array(ImmutableSet
						.of(new CqlColumnName("col 1"), new CqlColumnName("col 1"), new CqlColumnName("col 1231"),
								new CqlColumnName("2col 41"))).param("param 4").build());
	}

	@Test
	public void tesSimpleParameters() {
		assertEquals("refreshPage('param 1','param 2')",
				JsFunctionBuilder.function("refreshPage").param("param 1").param("param 2").build());
	}
}

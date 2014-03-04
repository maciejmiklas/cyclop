package org.cyclop.service.common;

import org.cyclop.model.exception.BeanValidationException;
import org.cyclop.service.common.CookieStorage.CookieName;
import org.cyclop.test.AbstractTestCase;
import org.junit.Test;

import javax.inject.Inject;

/** @author Maciej Miklas */
public class TestCookieStorage extends AbstractTestCase {

	@Inject
	private CookieStorage storage;

	@Test(expected = BeanValidationException.class)
	public void testReadCookie_Validation() {
		storage.readCookie(null);
	}

	@Test(expected = BeanValidationException.class)
	public void testReadCookieAsJson_Validation() {
		storage.readCookieAsJson(null, null);
	}

	@Test(expected = BeanValidationException.class)
	public void testStoreCookie_Validation() {
		storage.storeCookie(null, null);
	}

	@Test(expected = BeanValidationException.class)
	public void testStoreCookieAsJson_Validation() {
		storage.storeCookieAsJson(CookieName.cyclop_prefs, null);
	}

}

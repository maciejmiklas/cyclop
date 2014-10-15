package org.cyclop.service.security;

import static org.junit.Assert.assertTrue;

import java.lang.reflect.Field;
import java.net.InetAddress;
import java.util.Optional;

import javax.inject.Inject;

import org.cyclop.common.AppConfig;
import org.cyclop.model.exception.BeanValidationException;
import org.cyclop.test.AbstractTestCase;
import org.junit.After;
import org.junit.Test;
import org.springframework.util.ReflectionUtils;
import org.springframework.util.StopWatch;

public class TestBruteForceService extends AbstractTestCase {

	@Inject
	private AppConfig config;

	@Inject
	private BruteForceService service;

	@Override
	@After
	public void cleanUp() throws Exception {
		super.cleanUp();
		setConf("incorrectLoginDelayMs", 1000);
		setConf("incorrectLoginDelayMultiplikator", 1.5);
		setConf("incorrectLoginDelayResetMs", 600000);
		service.resetLoginFailed(Optional.of(InetAddress.getLocalHost()), Optional.of(InetAddress.getLocalHost()));
	}

	private void setConf(String method, Object val) {
		Field field = ReflectionUtils.findField(AppConfig.Login.class, method);
		ReflectionUtils.makeAccessible(field);
		ReflectionUtils.setField(field, config.login, val);
	}

	@Test(expected = BeanValidationException.class)
	public void testResetLoginFailed_Null() throws Exception {
		service.resetLoginFailed(null, null);
	}

	@Test(expected = BeanValidationException.class)
	public void testLoginFailed_Null_Message() throws Exception {
		service.loginFailed(null, Optional.of(InetAddress.getLocalHost()), Optional.of(InetAddress.getLocalHost()));
	}

	@Test(expected = BeanValidationException.class)
	public void testLoginFailed_Null_ClientIp() throws Exception {
		service.loginFailed(Optional.of("message ...."), null, Optional.of(InetAddress.getLocalHost()));
	}

	@Test(expected = BeanValidationException.class)
	public void testLoginFailed_Null_ProxyIp() throws Exception {
		service.loginFailed(Optional.of("message ...."), Optional.of(InetAddress.getLocalHost()), null);
	}

	@Test
	public void testLoginFailed_SingleWait() throws Exception {
		setConf("incorrectLoginDelayMs", 100);
		execLoginFailed(100);
	}

	@Test
	public void testLoginFailed_ThreeWaitsNoDelay() throws Exception {
		setConf("incorrectLoginDelayMs", 100);
		execLoginFailed(100);
		execLoginFailed(150);
		execLoginFailed(225);
	}

	@Test
	public void testLoginFailed_ThreeWaitsWithDelayAndReset() throws Exception {
		setConf("incorrectLoginDelayMs", 100);
		setConf("incorrectLoginDelayResetMs", 300);

		execLoginFailed(100);
		Thread.sleep(100);

		execLoginFailed(150);
		Thread.sleep(100);

		execLoginFailed(225);
		Thread.sleep(400);

		execLoginFailed(100);
	}

	private void execLoginFailed(int waitTime) throws Exception {
		StopWatch sw = new StopWatch();
		sw.start();
		service.loginFailed(Optional.of("error message 123..."), Optional.of(InetAddress.getLocalHost()),
				Optional.of(InetAddress.getLocalHost()));
		sw.stop();
		assertTrue(service
				.checkActive(Optional.of(InetAddress.getLocalHost()), Optional.of(InetAddress.getLocalHost())));
		assertTrue("Wait time: " + sw.getTotalTimeMillis(), sw.getTotalTimeMillis() >= waitTime - 50);
		assertTrue("Wait time: " + sw.getTotalTimeMillis(), sw.getTotalTimeMillis() <= waitTime + 50);
	}
}

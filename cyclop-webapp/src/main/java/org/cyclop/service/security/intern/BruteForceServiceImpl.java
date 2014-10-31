package org.cyclop.service.security.intern;

import java.net.InetAddress;
import java.util.Optional;
import java.util.concurrent.atomic.AtomicLong;

import javax.inject.Inject;
import javax.inject.Named;

import org.cyclop.common.AppConfig;
import org.cyclop.service.security.BruteForceService;
import org.cyclop.validation.EnableValidation;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

@Named
@EnableValidation
class BruteForceServiceImpl implements BruteForceService {
	private final static Logger LOG = LoggerFactory.getLogger(BruteForceServiceImpl.class);

	@Inject
	private AppConfig config;
	private AtomicLong blockTime = new AtomicLong();
	private long startBlocking = 0;

	@Override
	public boolean checkActive(Optional<InetAddress> clientIp, Optional<InetAddress> proxyIp) {
		return blockTime.get() > 0;
	}

	@Override
	public synchronized void resetLoginFailed(Optional<InetAddress> clientIp, Optional<InetAddress> proxyIp) {
		LOG.debug("Reseting login delay");
		startBlocking = 0;
		blockTime.set(0);
	}

	@Override
	public synchronized void loginFailed(Optional<String> errorMessage, Optional<InetAddress> clientIp,
			Optional<InetAddress> proxyIp) {
		if (config.login.blockDelayMs <= 0) {
			LOG.debug("Brute force protection is disabled");
			return;
		}

		long blockMs = calculateBlockTime();
		if (blockMs == 0) {
			return;
		}
		if (blockMs > 5000) {
			LOG.info("Incorrect login from client: {} with proxy {}, message: {} - blocking for {} ms",
					clientIp.orElse(null), proxyIp.orElse(null), errorMessage.orElse("-"), blockMs);
		}
		try {
			Thread.sleep(blockMs);
		} catch (InterruptedException e) {
			Thread.currentThread().interrupt();
		}
	}

	private long calculateBlockTime() {
		if (startBlocking == 0 || ((System.currentTimeMillis() - startBlocking) >= config.login.blockDelayResetMs)) {
			startBlocking = System.currentTimeMillis();
			blockTime.set(config.login.blockDelayMs);

		} else {
			blockTime.set(Math.min((int) (blockTime.get() * config.login.blockDelayMultiplikator),
					config.login.maxBlockMs));
			startBlocking = System.currentTimeMillis();
		}

		return blockTime.get();
	}
}

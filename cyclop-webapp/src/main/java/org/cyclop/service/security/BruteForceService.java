package org.cyclop.service.security;

import java.net.InetAddress;
import java.util.Optional;

import javax.validation.constraints.NotNull;

public interface BruteForceService {

	void loginFailed(@NotNull String errorMessage, @NotNull Optional<InetAddress> clientIp,
			@NotNull Optional<InetAddress> proxyIp);

	void resetLoginFailed(@NotNull Optional<InetAddress> clientIp, @NotNull Optional<InetAddress> proxyIp);

	boolean checkActive(@NotNull Optional<InetAddress> clientIp, @NotNull Optional<InetAddress> proxyIp);
}

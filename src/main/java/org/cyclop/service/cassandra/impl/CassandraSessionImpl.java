package org.cyclop.service.cassandra.impl;

import com.datastax.driver.core.Cluster;
import com.datastax.driver.core.HostDistance;
import com.datastax.driver.core.PoolingOptions;
import com.datastax.driver.core.Session;
import com.datastax.driver.core.ShutdownFuture;
import com.datastax.driver.core.SocketOptions;
import com.datastax.driver.core.exceptions.InvalidQueryException;
import net.jcip.annotations.NotThreadSafe;
import org.cyclop.common.AppConfig;
import org.cyclop.model.exception.AuthenticationRequiredException;
import org.cyclop.service.cassandra.CassandraSession;
import org.cyclop.validation.EnableValidation;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.annotation.Scope;
import org.springframework.context.annotation.ScopedProxyMode;

import javax.annotation.PreDestroy;
import javax.inject.Inject;
import javax.inject.Named;
import javax.validation.constraints.NotNull;

/** @author Maciej Miklas */
@NotThreadSafe
@Named
@Scope(value = "session", proxyMode = ScopedProxyMode.TARGET_CLASS)
@EnableValidation
public class CassandraSessionImpl implements CassandraSession {
	private final static Logger LOG = LoggerFactory.getLogger(CassandraSessionImpl.class);

	private Session session;

	@Inject
	private AppConfig appConfig;

	private CassandraVersion cassandraVersion;

	private Cluster cluster;

	public synchronized void authenticate(@NotNull String userName, @NotNull String password) {
		Cluster.Builder builder = Cluster.builder();
		for (String host : appConfig.cassandra.hosts.split("[,]")) {
			builder.addContactPoint(host);
		}
		builder.withCredentials(userName, password);

		if (appConfig.cassandra.useSsl) {
			builder.withSSL();
		}

		SocketOptions socketOptions = new SocketOptions();
		socketOptions.setConnectTimeoutMillis(appConfig.cassandra.timeoutMilis);

		PoolingOptions pooling = new PoolingOptions();
		pooling.setMaxConnectionsPerHost(HostDistance.LOCAL, 1);
		pooling.setCoreConnectionsPerHost(HostDistance.LOCAL, 1);
		pooling.setMaxSimultaneousRequestsPerConnectionThreshold(HostDistance.LOCAL, 1);
		pooling.setMinSimultaneousRequestsPerConnectionThreshold(HostDistance.LOCAL, 1);

		pooling.setMaxConnectionsPerHost(HostDistance.REMOTE, 1);
		pooling.setCoreConnectionsPerHost(HostDistance.REMOTE, 1);
		pooling.setMaxSimultaneousRequestsPerConnectionThreshold(HostDistance.REMOTE, 1);
		pooling.setMinSimultaneousRequestsPerConnectionThreshold(HostDistance.REMOTE, 1);

		cluster = builder.withPort(appConfig.cassandra.port).withSocketOptions(socketOptions)
				.withPoolingOptions(pooling).build();
		session = cluster.connect();

		cassandraVersion = determineVersion(session);
	}

	private CassandraVersion determineVersion(Session session) {
		CassandraVersion ver = CassandraVersion.VER_2_x;

		// this way to check version sucks and works at the same time ....
		try {
			session.execute("select type FROM system.schema_columns LIMIT 1 ALLOW FILTERING");
		} catch (InvalidQueryException e) {
			ver = CassandraVersion.VER_1_x;
		}
		return ver;
	}

	protected CassandraVersion getCassandraVersion() {
		checkAuthenticated();
		return cassandraVersion;
	}

	protected Session getSession() {
		checkAuthenticated();
		return session;
	}

	public synchronized void close() {
		if (cluster != null) {
			try {
				ShutdownFuture shf = cluster.shutdown();

				// wait for cluster shutdown without timeout.
				// This will prevent cyclop for opening to many cluster
				// connections
				shf.get();
			} catch (Exception e) {
				LOG.warn("Error while shutting down the cluster", e);
			}
			cluster = null;
			session = null;
		}
	}

	private void checkAuthenticated() {
		if (!isOpen()) {
			throw new AuthenticationRequiredException("Cassandra session not found");
		}
	}

	@Override
	public synchronized boolean isOpen() {
		return session != null;

	}

	@PreDestroy
	public void cleanup() {
		close();
	}

}

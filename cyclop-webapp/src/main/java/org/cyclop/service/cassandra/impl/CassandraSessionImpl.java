/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.cyclop.service.cassandra.impl;

import javax.annotation.PreDestroy;
import javax.inject.Inject;
import javax.inject.Named;
import javax.validation.constraints.NotNull;

import net.jcip.annotations.NotThreadSafe;

import org.cyclop.common.AppConfig;
import org.cyclop.model.exception.AuthenticationRequiredException;
import org.cyclop.service.cassandra.CassandraSession;
import org.cyclop.validation.EnableValidation;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.annotation.Scope;
import org.springframework.context.annotation.ScopedProxyMode;

import com.datastax.driver.core.Cluster;
import com.datastax.driver.core.HostDistance;
import com.datastax.driver.core.PoolingOptions;
import com.datastax.driver.core.Session;
import com.datastax.driver.core.SocketOptions;
import com.datastax.driver.core.exceptions.InvalidQueryException;

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
	if (cluster != null) {
	    return;
	}
	Cluster.Builder builder = Cluster.builder();
	for (String host : appConfig.cassandra.hosts.split("[,]")) {
	    builder.addContactPoint(host);
	}
	builder.withCredentials(userName, password);

	if (appConfig.cassandra.useSsl) {
	    builder.withSSL();
	}

	SocketOptions socketOptions = new SocketOptions();
	socketOptions.setConnectTimeoutMillis(appConfig.cassandra.timeoutMillis);

	PoolingOptions pooling = new PoolingOptions();
	pooling.setCoreConnectionsPerHost(HostDistance.LOCAL, appConfig.cassandra.coreConnectionsPerHost);
	pooling.setMaxConnectionsPerHost(HostDistance.LOCAL, appConfig.cassandra.maxConnectionsPerHost);
	pooling.setMinSimultaneousRequestsPerConnectionThreshold(
		HostDistance.LOCAL,
		appConfig.cassandra.minSimultaneousRequestsPerConnectionThreshold);
	pooling.setMaxSimultaneousRequestsPerConnectionThreshold(
		HostDistance.LOCAL,
		appConfig.cassandra.maxSimultaneousRequestsPerConnectionThreshold);

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
	}
	catch (InvalidQueryException e) {
	    ver = CassandraVersion.VER_1_x;
	}
	return ver;
    }

    protected CassandraVersion getCassandraVersion() {
	checkAuthenticated();
	return cassandraVersion;
    }

    @Override
    public Session getSession() {
	checkAuthenticated();
	return session;
    }

    public synchronized void close() {
	if (cluster != null) {
	    try {
		cluster.close();
	    }
	    catch (Exception e) {
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

package org.cyclop.service.cassandra.impl;

import com.datastax.driver.core.Cluster;
import com.datastax.driver.core.Session;
import com.datastax.driver.core.exceptions.InvalidQueryException;
import javax.annotation.concurrent.NotThreadSafe;
import javax.inject.Inject;
import javax.inject.Named;
import org.cyclop.common.AppConfig;
import org.cyclop.model.exception.ServiceException;
import org.cyclop.service.cassandra.CassandraSession;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.annotation.Scope;
import org.springframework.context.annotation.ScopedProxyMode;

/**
 * @author Maciej Miklas
 */
@NotThreadSafe
@Named
@Scope(value = "session", proxyMode = ScopedProxyMode.TARGET_CLASS)
class CassandraSessionImpl implements CassandraSession {
    private final static Logger LOG = LoggerFactory.getLogger(CassandraSessionImpl.class);

    private Session session;

    @Inject
    private AppConfig appConfig;

    private CassandraVersion cassandraVersion;

    public void authenticate(String userName, String password) {
        if (userName == null || password == null) {
            throw new IllegalArgumentException("provide user name and password");
        }

        Cluster.Builder builder = Cluster.builder();
        for (String host : appConfig.cassandra.hosts.split("[,]")) {
            builder.addContactPoint(host);
        }
        builder.withCredentials(userName, password);

        if (appConfig.cassandra.useSsl) {
            builder.withSSL();
        }
        builder.withPort(appConfig.cassandra.port);
        builder.socketOptions().setConnectTimeoutMillis(appConfig.cassandra.timeoutMilis);
        builder.socketOptions().setReadTimeoutMillis(appConfig.cassandra.timeoutMilis);
        Cluster cluster = builder.build();
        session = cluster.connect();

        cassandraVersion = determineVersion(session);
    }

    private CassandraVersion determineVersion(Session session) {
        CassandraVersion ver = CassandraVersion.VER_2_x;

        // this sucks and works at the same time ....
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

    public void close() {
        if (session != null) {
            try {
                session.shutdown();
            } catch (Exception e) {
                LOG.warn("Error closing session", e);
            }
        }
    }

    private void checkAuthenticated() {
        if (session == null) {
            throw new ServiceException("Cassandra session not found");
        }
    }
}

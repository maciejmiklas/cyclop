package org.cyclop.test;

import java.io.File;
import java.io.IOException;

import org.apache.cassandra.io.util.FileUtils;
import org.cassandraunit.utils.EmbeddedCassandraServerHelper;

import com.datastax.driver.core.Cluster;
import com.datastax.driver.core.Session;

import static junit.framework.Assert.assertNotNull;

/**
 * @author Maciej Miklas
 */
public class EmbeddedCassandra {

    private Cluster cluster;

    private Session session;

    public void start() throws Exception {
        EmbeddedCassandraServerHelper.startEmbeddedCassandra("/cassandra.yaml");

        cluster = Cluster.builder().addContactPoints("localhost").withPort(9042).build();
        assertNotNull(cluster);

        session = cluster.connect();
        assertNotNull(session);

        createTestData(session);
    }

    private void createTestData(Session session) {
        session.execute("CREATE KEYSPACE CqlDemo WITH replication = {'class': 'SimpleStrategy', " +
                "'replication_factor' : 1}");
        session.execute("USE CqlDemo");
        session.execute("CREATE TABLE MyBooks (id UUID PRIMARY KEY,title TEXT,genre TEXT,publishDate TIMESTAMP,description TEXT,authors SET<TEXT>,price MAP<TEXT,DOUBLE>,pages INT)");
        session.execute("ALTER TABLE MyBooks ADD paperType varchar");
        session.execute("ALTER TABLE MyBooks ADD dynamicColumn1 varchar");
        session.execute("ALTER TABLE MyBooks ADD dynamicColumn2 varchar");
        session.execute("ALTER TABLE MyBooks ADD dynamicColumn3 varchar");
        session.execute("ALTER TABLE MyBooks ADD idx int");
        session.execute("CREATE INDEX ON MyBooks(description)");
        session.execute("CREATE INDEX ON MyBooks(publishDate)");
        session.execute("CREATE INDEX ON MyBooks(genre)");
        session.execute("CREATE INDEX ON MyBooks(pages)");

        session.execute("create table CompoundTest (id uuid, id2 int, id3 text, deesc text, primary key(id, id2, id3))");
    }

    public Cluster getCluster() {
        return cluster;
    }

    public void stop() throws Exception {
        cluster.shutdown();
    }

    private static void rmdir(String dir) throws IOException {
        File dirFile = new File(dir);
        if (dirFile.exists()) {
            FileUtils.deleteRecursive(new File(dir));
        }
    }

    public Session getSession() {
        return session;
    }
}

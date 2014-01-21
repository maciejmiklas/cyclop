package org.cyclop.service.cassandra;

/**
 * @author Maciej Miklas
 */
public interface CassandraSession {

    void authenticate(String userName, String password);

    void close();
}

package org.cyclop.service.cassandra;

import javax.validation.constraints.NotNull;

/** @author Maciej Miklas */
public interface CassandraSession extends AutoCloseable {

	void authenticate(@NotNull String userName, @NotNull String password);

	void close();

	boolean isOpen();
}

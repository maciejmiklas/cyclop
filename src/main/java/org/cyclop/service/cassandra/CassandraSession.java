package org.cyclop.service.cassandra;

import javax.validation.constraints.NotNull;

/** @author Maciej Miklas */
public interface CassandraSession {

	void authenticate(@NotNull String userName, @NotNull String password);

	void close();
}

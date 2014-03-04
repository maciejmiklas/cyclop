package org.cyclop.service.completion;

import org.cyclop.model.ContextCqlCompletion;
import org.cyclop.model.CqlQuery;

import javax.validation.constraints.NotNull;

/** @author Maciej Miklas */
public interface CompletionService {

	/** @return cql markers are: "alter table", "select" or "create keyspace" - columnType of cql query */
	@NotNull
	ContextCqlCompletion findInitialCompletion();

	/**
	 * @param cursorPosition
	 * 		starts from 0
	 */
	@NotNull
	ContextCqlCompletion findCompletion(@NotNull CqlQuery cqlQuery, int cursorPosition);

	@NotNull
	ContextCqlCompletion findCompletion(@NotNull CqlQuery cqlQuery);
}

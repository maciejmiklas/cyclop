package org.cyclop.service.completion;

import org.cyclop.model.ContextCqlCompletion;
import org.cyclop.model.CqlQuery;

/**
 * @author Maciej Miklas
 */
public interface CompletionService {
    /**
     * @return cql markers are: "alter table", "select" or "create keyspace" - columnType of cql query
     */
    ContextCqlCompletion findInitialCompletion();

    ContextCqlCompletion findCompletion(CqlQuery cqlQuery, int cursorPosition);
}

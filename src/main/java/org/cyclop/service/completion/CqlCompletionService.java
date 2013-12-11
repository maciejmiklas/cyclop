package org.cyclop.service.completion;

import org.cyclop.service.model.ContextCqlCompletion;
import org.cyclop.service.model.CqlQuery;

/**
 * @author Maciej Miklas
 */
public interface CqlCompletionService {
    /**
     * @return cql markers are: "alter table", "select" or "create keyspace" - columnType of cql query
     */
    ContextCqlCompletion findInitialCompletion();

    ContextCqlCompletion findCompletion(CqlQuery cqlQuery, int cursorPosition);
}

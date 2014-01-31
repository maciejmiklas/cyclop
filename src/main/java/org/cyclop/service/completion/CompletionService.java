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

    /**
     * @param cursorPosition starts from 0
     */
    ContextCqlCompletion findCompletion(CqlQuery cqlQuery, int cursorPosition);

    ContextCqlCompletion findCompletion(CqlQuery cqlQuery);
}

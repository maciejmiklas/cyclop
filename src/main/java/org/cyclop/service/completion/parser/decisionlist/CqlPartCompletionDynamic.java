package org.cyclop.service.completion.parser.decisionlist;

import org.cyclop.model.CqlQuery;

/**
 * @author Maciej Miklas
 */
public interface CqlPartCompletionDynamic extends CqlPartCompletion {

    /**
     * @return offset when competition of this completion ends, or -1 if it cannot be applied
     */
    int canApply(CqlQuery query, int queryPosition);
}

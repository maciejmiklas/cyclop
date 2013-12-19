package org.cyclop.service.completion.parser;

import org.cyclop.model.CqlQuery;

/**
 * @author Maciej Miklas
 */
public interface OffsetBasedCompletion extends CqlPartCompletion {

    /**
     * @return offset when competition of this completion ends, or -1 if it cannot be applied
     */
    int canApply(CqlQuery query, int queryPosition);
}

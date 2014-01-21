package org.cyclop.service.completion.impl.parser;

import org.cyclop.model.CqlKeyword;
import org.cyclop.model.CqlQueryName;

/**
 * @author Maciej Miklas
 */
public interface DecisionListSupport {

    /**
     * @return two dimensionless array: x - next completion, y - optional compilations for current position in x
     */
    CqlPartCompletion[][] getDecisionList();

    CqlKeyword supports();

    CqlQueryName queryName();
}

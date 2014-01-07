package org.cyclop.service.completion.parser;

import org.cyclop.model.CqlKeyword;
import org.cyclop.model.CqlQueryName;

/**
 * @author Maciej Miklas
 */
public interface DecisionListSupport {

    CqlPartCompletion[] getDecisionList();

    CqlKeyword supports();

    CqlQueryName queryName();
}

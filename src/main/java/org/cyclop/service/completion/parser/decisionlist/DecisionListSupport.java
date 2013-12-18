package org.cyclop.service.completion.parser.decisionlist;

import org.cyclop.model.CqlKeyword;
import org.cyclop.model.CqlQueryType;

/**
 * @author Maciej Miklas
 */
public interface DecisionListSupport {

    CqlPartCompletion[] getDecisionList();

    CqlKeyword supports();

    CqlQueryType queryType();
}

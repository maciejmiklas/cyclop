package org.cyclop.service.completion.parser.decisionlist;

import org.cyclop.service.model.CqlKeyword;
import org.cyclop.service.model.CqlQueryType;

/**
 * @author Maciej Miklas
 */
public interface DecisionListSupport {

    CqlPartCompletion[] getDecisionList();

    CqlKeyword supports();

    CqlQueryType queryType();
}

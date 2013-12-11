package org.cyclop.service.completion.parser.decisionlist;

import org.cyclop.service.model.CqlCompletion;
import org.cyclop.service.model.CqlQuery;

/**
 * @author Maciej Miklas
 */
public interface CqlPartCompletion {

    CqlCompletion getCompletion(CqlQuery query);
}

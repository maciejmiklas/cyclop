package org.cyclop.service.completion.parser;

import org.cyclop.model.CqlCompletion;
import org.cyclop.model.CqlQuery;

/**
 * @author Maciej Miklas
 */
public interface CqlPartCompletion {

    CqlCompletion getCompletion(CqlQuery query);
}

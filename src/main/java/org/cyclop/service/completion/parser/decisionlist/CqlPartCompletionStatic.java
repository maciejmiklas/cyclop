package org.cyclop.service.completion.parser.decisionlist;

import org.cyclop.model.CqlPart;

/**
 * @author Maciej Miklas
 */
public interface CqlPartCompletionStatic extends CqlPartCompletion {

    CqlPart[] startMarkers();
}

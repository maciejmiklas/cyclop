package org.cyclop.service.completion.parser.decisionlist.dropkeyspace;

import org.cyclop.model.CqlKeyword;
import org.cyclop.model.CqlPart;
import org.cyclop.service.completion.parser.decisionlist.NotSupportedCompletion;

import javax.inject.Named;

/**
 * @author Maciej Miklas
 */
@Named("dropkeyspace.DropCompletion")
public class DropCompletion extends NotSupportedCompletion {

    private final CqlPart[] startMarker = new CqlPart[]{new CqlKeyword("drop")};

    @Override
    public CqlPart[] startMarkers() {
        return startMarker;
    }

    @Override
    protected String getNotSupportedText() {
        return "drop keyspace";
    }
}

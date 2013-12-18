package org.cyclop.service.completion.parser.decisionlist.truncate;

import org.cyclop.model.CqlPart;
import org.cyclop.service.completion.parser.decisionlist.NotSupportedCompletion;

import javax.inject.Named;

/**
 * @author Maciej Miklas
 */
@Named("truncate.TruncateCompletion")
public class TruncateCompletion extends NotSupportedCompletion {

    private final CqlPart[] startMarker = new CqlPart[]{new CqlPart("truncate")};

    @Override
    public CqlPart[] startMarkers() {
        return startMarker;
    }

    @Override
    protected String getNotSupportedText() {
        return "truncate";
    }
}

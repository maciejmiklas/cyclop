package org.cyclop.service.completion.parser.decisionlist.batch;

import org.cyclop.model.CqlNotSupported;
import org.cyclop.model.CqlPart;
import org.cyclop.service.completion.parser.decisionlist.NotSupportedCompletion;

import javax.inject.Named;

/**
 * @author Maciej Miklas
 */
@Named("batch.BatchCompletion")
public class BatchCompletion extends NotSupportedCompletion {

    private final CqlPart[] startMarker = new CqlPart[]{new CqlNotSupported("batch")};

    @Override
    public CqlPart[] startMarkers() {
        return startMarker;
    }

    @Override
    protected String getNotSupportedText() {
        return "batch";
    }
}

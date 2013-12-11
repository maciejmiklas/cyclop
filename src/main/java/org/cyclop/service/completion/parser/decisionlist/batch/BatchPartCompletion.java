package org.cyclop.service.completion.parser.decisionlist.batch;

import javax.inject.Named;
import org.cyclop.service.completion.parser.decisionlist.NotSupportedCompletion;
import org.cyclop.service.model.CqlNotSupported;
import org.cyclop.service.model.CqlPart;

/**
 * @author Maciej Miklas
 */
@Named("batch.BatchPartCompletion")
public class BatchPartCompletion extends NotSupportedCompletion {

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

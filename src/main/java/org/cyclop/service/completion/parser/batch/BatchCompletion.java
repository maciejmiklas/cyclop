package org.cyclop.service.completion.parser.batch;

import org.cyclop.model.CqlNotSupported;
import org.cyclop.model.CqlPart;
import org.cyclop.service.completion.parser.NotSupportedCompletion;

import javax.inject.Named;

/**
 * @author Maciej Miklas
 */
@Named("batch.BatchCompletion")
public class BatchCompletion extends NotSupportedCompletion {

    private final static CqlPart SM = new CqlNotSupported("batch");

    @Override
    public CqlPart startMarker() {
        return SM;
    }

    @Override
    protected String getNotSupportedText() {
        return "batch";
    }
}

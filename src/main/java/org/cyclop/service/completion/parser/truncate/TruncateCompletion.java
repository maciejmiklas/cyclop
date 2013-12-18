package org.cyclop.service.completion.parser.truncate;

import org.cyclop.model.CqlPart;
import org.cyclop.service.completion.parser.NotSupportedCompletion;

import javax.inject.Named;

/**
 * @author Maciej Miklas
 */
@Named("truncate.TruncateCompletion")
public class TruncateCompletion extends NotSupportedCompletion {

    private final static CqlPart SM = new CqlPart("truncate");

    @Override
    public CqlPart startMarker() {
        return SM;
    }

    @Override
    protected String getNotSupportedText() {
        return "truncate";
    }
}

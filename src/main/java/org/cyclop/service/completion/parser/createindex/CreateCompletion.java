package org.cyclop.service.completion.parser.createindex;

import org.cyclop.model.CqlNotSupported;
import org.cyclop.model.CqlPart;
import org.cyclop.service.completion.parser.NotSupportedCompletion;

import javax.inject.Named;

/**
 * @author Maciej Miklas
 */
@Named("createindex.CreateCompletion")
public class CreateCompletion extends NotSupportedCompletion {

    private final static CqlPart SM = new CqlNotSupported("create");

    @Override
    public CqlPart startMarker() {
        return SM;
    }

    @Override
    protected String getNotSupportedText() {
        return "create index";
    }
}

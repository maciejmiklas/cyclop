package org.cyclop.service.completion.parser.createkeyspace;

import org.cyclop.model.CqlNotSupported;
import org.cyclop.model.CqlPart;
import org.cyclop.service.completion.parser.NotSupportedCompletion;

import javax.inject.Named;

/**
 * @author Maciej Miklas
 */
@Named("createkeyspace.CreateCompletion")
public class CreateCompletion extends NotSupportedCompletion {

    private final CqlPart SM = new CqlNotSupported("create");

    @Override
    public CqlPart startMarker() {
        return SM;
    }

    @Override
    protected String getNotSupportedText() {
        return "create keyspace";
    }
}

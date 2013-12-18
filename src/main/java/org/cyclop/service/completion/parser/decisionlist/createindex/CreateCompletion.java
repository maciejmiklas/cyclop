package org.cyclop.service.completion.parser.decisionlist.createindex;

import org.cyclop.model.CqlNotSupported;
import org.cyclop.model.CqlPart;
import org.cyclop.service.completion.parser.decisionlist.NotSupportedCompletion;

import javax.inject.Named;

/**
 * @author Maciej Miklas
 */
@Named("createindex.CreateCompletion")
public class CreateCompletion extends NotSupportedCompletion {

    private final CqlPart[] startMarker = new CqlPart[]{new CqlNotSupported("create")};

    @Override
    public CqlPart[] startMarkers() {
        return startMarker;
    }

    @Override
    protected String getNotSupportedText() {
        return "create index";
    }
}

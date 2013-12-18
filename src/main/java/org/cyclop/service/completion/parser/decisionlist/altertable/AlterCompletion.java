package org.cyclop.service.completion.parser.decisionlist.altertable;

import org.cyclop.model.CqlNotSupported;
import org.cyclop.model.CqlPart;
import org.cyclop.service.completion.parser.decisionlist.NotSupportedCompletion;

import javax.inject.Named;

/**
 * @author Maciej Miklas
 */
@Named("altertable.AlterCompletion")
public class AlterCompletion extends NotSupportedCompletion {

    private final CqlPart[] startMarker = new CqlPart[]{new CqlNotSupported("alter")};

    @Override
    public CqlPart[] startMarkers() {
        return startMarker;
    }

    @Override
    protected String getNotSupportedText() {
        return "alter table";
    }
}

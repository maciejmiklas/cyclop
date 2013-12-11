package org.cyclop.service.completion.parser.decisionlist.delete;

import javax.inject.Named;
import org.cyclop.service.completion.parser.decisionlist.NotSupportedCompletion;
import org.cyclop.service.model.CqlNotSupported;
import org.cyclop.service.model.CqlPart;

/**
 * @author Maciej Miklas
 */
@Named("delete.DeletePartCompletion")
public class DeletePartCompletion extends NotSupportedCompletion {

    private final CqlPart[] startMarker = new CqlPart[]{new CqlNotSupported("delete")};

    @Override
    public CqlPart[] startMarkers() {
        return startMarker;
    }

    @Override
    protected String getNotSupportedText() {
        return "delete";
    }
}

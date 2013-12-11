package org.cyclop.service.completion.parser.decisionlist.update;

import javax.inject.Named;
import org.cyclop.service.completion.parser.decisionlist.NotSupportedCompletion;
import org.cyclop.service.model.CqlPart;

/**
 * @author Maciej Miklas
 */
@Named("update.UpdatePartCompletion")
public class UpdatePartCompletion extends NotSupportedCompletion {

    private final CqlPart[] startMarker = new CqlPart[]{new CqlPart("update")};

    @Override
    public CqlPart[] startMarkers() {
        return startMarker;
    }


    @Override
    protected String getNotSupportedText() {
        return "update";
    }
}

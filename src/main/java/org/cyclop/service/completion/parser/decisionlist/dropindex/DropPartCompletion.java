package org.cyclop.service.completion.parser.decisionlist.dropindex;

import javax.inject.Named;
import org.cyclop.service.completion.parser.decisionlist.NotSupportedCompletion;
import org.cyclop.service.model.CqlKeyword;
import org.cyclop.service.model.CqlPart;

/**
 * @author Maciej Miklas
 */
@Named("dropindex.DropPartCompletion")
public class DropPartCompletion extends NotSupportedCompletion {

    private final CqlPart[] startMarker = new CqlPart[]{new CqlKeyword("drop")};

    @Override
    public CqlPart[] startMarkers() {
        return startMarker;
    }

    @Override
    protected String getNotSupportedText() {
        return "drop index";
    }
}

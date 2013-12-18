package org.cyclop.service.completion.parser.droptable;

import org.cyclop.model.CqlCompletion;
import org.cyclop.model.CqlKeyword;
import org.cyclop.model.CqlPart;
import org.cyclop.model.CqlQuery;
import org.cyclop.service.completion.parser.CqlPartCompletionStatic;
import org.cyclop.service.completion.parser.DecisionHelper;

import javax.inject.Inject;
import javax.inject.Named;

import static org.cyclop.model.CqlKeywords.DROP;

/**
 * @author Maciej Miklas
 */
@Named("droptable.DropCompletion")
public class DropCompletion implements CqlPartCompletionStatic {

    private final CqlPart[] startMarker = new CqlPart[]{new CqlKeyword("drop")};

    @Inject
    private DecisionHelper decisionHelper;

    @Override
    public CqlPart[] startMarkers() {
        return startMarker;
    }

    @Override
    public CqlCompletion getCompletion(CqlQuery query) {
        CqlCompletion completion = decisionHelper.computeTableNameCompletion(DROP, query);
        return completion;
    }

}

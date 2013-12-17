package org.cyclop.service.completion.parser.decisionlist.droptable;

import javax.inject.Inject;
import javax.inject.Named;

import org.cyclop.common.QueryHelper;
import org.cyclop.service.completion.parser.decisionlist.CqlPartCompletionStatic;
import org.cyclop.service.completion.parser.decisionlist.NotSupportedCompletion;
import org.cyclop.service.completion.parser.decisionlist.util.DecisionHelper;
import org.cyclop.service.model.CqlCompletion;
import org.cyclop.service.model.CqlKeyword;
import org.cyclop.service.model.CqlPart;
import org.cyclop.service.model.CqlQuery;

/**
 * @author Maciej Miklas
 */
@Named
public class DropPartCompletion implements CqlPartCompletionStatic {

    private final CqlPart[] startMarker = new CqlPart[]{new CqlKeyword("drop")};

    @Inject
    private DecisionHelper decisionHelper;

    @Override
    public CqlPart[] startMarkers() {
        return startMarker;
    }

    @Override
    public CqlCompletion getCompletion(CqlQuery query) {
        CqlCompletion completion = decisionHelper.computeTableNameCompletion(QueryHelper
                .KW_DROP, query);
        return completion;
    }

}

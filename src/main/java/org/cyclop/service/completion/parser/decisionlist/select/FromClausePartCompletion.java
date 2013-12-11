package org.cyclop.service.completion.parser.decisionlist.select;

import javax.inject.Inject;
import javax.inject.Named;
import org.cyclop.common.QueryHelper;
import org.cyclop.service.completion.parser.decisionlist.CqlPartCompletionStatic;
import org.cyclop.service.completion.parser.decisionlist.util.DecisionHelper;
import org.cyclop.service.model.CqlCompletion;
import org.cyclop.service.model.CqlKeyword;
import org.cyclop.service.model.CqlPart;
import org.cyclop.service.model.CqlQuery;

/**
 * @author Maciej Miklas
 */
@Named("select.FromClausePartCompletion")
public class FromClausePartCompletion implements CqlPartCompletionStatic {

    @Inject
    private DecisionHelper decisionHelper;

    private final CqlPart[] startMarker = new CqlPart[]{new CqlKeyword("from")};

    @Override
    public CqlCompletion getCompletion(CqlQuery query) {
        CqlCompletion completion = decisionHelper.computeTableNameCompletionWithKeyspaceInQuery(QueryHelper
                .KW_SELECT, query);
        if (completion == null) {
            completion = decisionHelper.computeTableNameCompletionWithoutKeyspaceInQuery();
        }
        return completion;
    }


    @Override
    public CqlPart[] startMarkers() {
        return startMarker;
    }

}

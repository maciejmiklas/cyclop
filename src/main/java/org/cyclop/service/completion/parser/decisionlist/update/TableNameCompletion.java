package org.cyclop.service.completion.parser.decisionlist.update;

import org.cyclop.model.CqlCompletion;
import org.cyclop.model.CqlKeyword;
import org.cyclop.model.CqlPart;
import org.cyclop.model.CqlQuery;
import org.cyclop.service.completion.parser.decisionlist.CqlPartCompletionStatic;
import org.cyclop.service.completion.parser.decisionlist.util.DecisionHelper;

import javax.inject.Inject;
import javax.inject.Named;

import static org.cyclop.model.CqlKeywords.UPDATE;

/**
 * @author Maciej Miklas
 */
@Named("update.TableNameCompletion")
public class TableNameCompletion implements CqlPartCompletionStatic {

    @Inject
    private DecisionHelper decisionHelper;

    private final CqlPart[] startMarker = new CqlPart[]{new CqlKeyword("update")};

    @Override
    public CqlCompletion getCompletion(CqlQuery query) {
        CqlCompletion completion;
        completion = decisionHelper.computeTableNameCompletion(UPDATE, query);
        return completion;
    }

    @Override
    public CqlPart[] startMarkers() {
        return startMarker;
    }

}

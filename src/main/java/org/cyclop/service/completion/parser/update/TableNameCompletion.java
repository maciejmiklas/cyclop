package org.cyclop.service.completion.parser.update;

import org.cyclop.model.CqlCompletion;
import org.cyclop.model.CqlKeyword;
import org.cyclop.model.CqlPart;
import org.cyclop.model.CqlQuery;
import org.cyclop.service.completion.parser.CqlPartCompletionStatic;
import org.cyclop.service.completion.parser.DecisionHelper;

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

    private final static CqlPart SM = new CqlKeyword("update");

    @Override
    public CqlCompletion getCompletion(CqlQuery query) {
        CqlCompletion completion;
        completion = decisionHelper.computeTableNameCompletion(UPDATE, query);
        return completion;
    }

    @Override
    public CqlPart startMarker() {
        return SM;
    }

}

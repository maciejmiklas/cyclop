package org.cyclop.service.completion.parser.decisionlist.insert;

import org.cyclop.model.CqlCompletion;
import org.cyclop.model.CqlKeyword;
import org.cyclop.model.CqlPart;
import org.cyclop.model.CqlQuery;
import org.cyclop.service.completion.parser.decisionlist.CqlPartCompletionStatic;
import org.cyclop.service.completion.parser.decisionlist.util.DecisionHelper;

import javax.inject.Inject;
import javax.inject.Named;

import static org.cyclop.model.CqlKeywords.FROM;

/**
 * @author Maciej Miklas
 */
@Named("insert.TableNameCompletion")
public class TableNameCompletion implements CqlPartCompletionStatic {

    @Inject
    private DecisionHelper decisionHelper;

    private final CqlPart[] startMarker = new CqlPart[]{new CqlKeyword("insert into")};

    @Override
    public CqlCompletion getCompletion(CqlQuery query) {
        CqlCompletion completion = decisionHelper.computeTableNameCompletion(FROM, query);
        return completion;
    }

    @Override
    public CqlPart[] startMarkers() {
        return startMarker;
    }

}

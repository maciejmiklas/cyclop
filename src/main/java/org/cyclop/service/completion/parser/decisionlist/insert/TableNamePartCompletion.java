package org.cyclop.service.completion.parser.decisionlist.insert;

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
@Named("insert.TableNamePartCompletion")
public class TableNamePartCompletion implements CqlPartCompletionStatic {

    @Inject
    private DecisionHelper decisionHelper;

    private final CqlPart[] startMarker = new CqlPart[]{new CqlKeyword("insert into")};

    @Override
    public CqlCompletion getCompletion(CqlQuery query) {
        CqlCompletion completion = decisionHelper.computeTableNameCompletion(QueryHelper.KW_INSERT, query);
        return completion;
    }

    @Override
    public CqlPart[] startMarkers() {
        return startMarker;
    }

}

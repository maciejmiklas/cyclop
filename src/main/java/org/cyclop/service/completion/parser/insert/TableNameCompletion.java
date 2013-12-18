package org.cyclop.service.completion.parser.insert;

import org.cyclop.model.CqlCompletion;
import org.cyclop.model.CqlKeyword;
import org.cyclop.model.CqlPart;
import org.cyclop.model.CqlQuery;
import org.cyclop.service.completion.parser.CqlPartCompletionStatic;
import org.cyclop.service.completion.parser.DecisionHelper;

import javax.inject.Inject;
import javax.inject.Named;

import static org.cyclop.model.CqlKeywords.INSERT;

/**
 * @author Maciej Miklas
 */
@Named("insert.TableNameCompletion")
public class TableNameCompletion implements CqlPartCompletionStatic {

    @Inject
    private DecisionHelper decisionHelper;

    private final static CqlPart SM = new CqlKeyword("insert into");

    @Override
    public CqlCompletion getCompletion(CqlQuery query) {
        CqlCompletion completion = decisionHelper.computeTableNameCompletion(INSERT, query);
        return completion;
    }

    @Override
    public CqlPart startMarker() {
        return SM;
    }

}

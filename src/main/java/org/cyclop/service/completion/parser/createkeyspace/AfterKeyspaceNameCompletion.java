package org.cyclop.service.completion.parser.createkeyspace;

import javax.annotation.PostConstruct;
import javax.inject.Named;
import org.cyclop.model.CqlCompletion;
import org.cyclop.model.CqlKeyword;
import org.cyclop.model.CqlQuery;
import org.cyclop.service.completion.parser.OffsetBasedCompletion;

/**
 * @author Maciej Miklas
 */
@Named("createkeyspace.AfterKeyspaceNameCompletion")
public class AfterKeyspaceNameCompletion implements OffsetBasedCompletion {

    private CqlCompletion completion;

    @PostConstruct
    public void init() {
        completion = CqlCompletion.Builder.naturalOrder().all(CqlKeyword.Def.WITH.value).all(CqlKeyword.Def
                .IF_NOT_EXISTS.value).build();
    }

    @Override
    public CqlCompletion getCompletion(CqlQuery query) {
        return completion;
    }

    @Override
    public int canApply(CqlQuery query, int queryPosition) {
        String cqlLc = query.cqlLc;
        int cqlLcLen = query.cqlLc.length();

        int indCreate = cqlLc.indexOf(CqlKeyword.Def.CREATE_KEYSPACE.value + " ", queryPosition);
        if (indCreate + 1 >= cqlLcLen) {
            return -1;
        }

        int indLastSpace = cqlLc.indexOf(' ', indCreate + 1);

        return indLastSpace;
    }
}

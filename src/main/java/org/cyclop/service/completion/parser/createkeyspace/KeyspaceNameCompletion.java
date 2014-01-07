package org.cyclop.service.completion.parser.createkeyspace;

import javax.inject.Named;
import org.cyclop.model.CqlCompletion;
import org.cyclop.model.CqlKeyword;
import org.cyclop.model.CqlQuery;
import org.cyclop.service.completion.parser.MarkerBasedCompletion;

/**
 * @author Maciej Miklas
 */
@Named("createkeyspace.KeyspaceNameCompletion")
public class KeyspaceNameCompletion extends MarkerBasedCompletion {

    public KeyspaceNameCompletion() {
        super(CqlKeyword.Def.CREATE_KEYSPACE.value);
    }

    @Override
    public CqlCompletion getCompletion(CqlQuery query) {
        return CqlCompletion.Builder.naturalOrder().all(CqlKeyword.Def.IF_NOT_EXISTS.value).build();
    }

}
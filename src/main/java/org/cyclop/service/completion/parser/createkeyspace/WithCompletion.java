package org.cyclop.service.completion.parser.createkeyspace;

import javax.inject.Named;
import org.cyclop.model.CqlCompletion;
import org.cyclop.model.CqlKeyword;
import org.cyclop.service.completion.parser.template.StaticMarkerBasedCompletion;

/**
 * @author Maciej Miklas
 */
@Named("createkeyspace.WithCompletion")
public class WithCompletion extends StaticMarkerBasedCompletion {

    public WithCompletion() {
        super(CqlKeyword.Def.WITH.value, CqlCompletion.Builder.naturalOrder().all(CqlKeyword.Def.VALUES.value).all
                (CqlKeyword.Def.REPLICATION.value).all(CqlKeyword.Def.DURABLE_WRITES.value).build());
    }

}

package org.cyclop.service.completion.impl.parser.createkeyspace;

import com.google.common.base.Objects;
import javax.inject.Named;
import org.cyclop.model.CqlCompletion;
import org.cyclop.model.CqlKeyword;
import org.cyclop.service.completion.impl.parser.template.StaticMarkerBasedCompletion;

/**
 * @author Maciej Miklas
 */
@Named("createkeyspace.WithCompletion")
class WithCompletion extends StaticMarkerBasedCompletion {

    public WithCompletion() {
        super(CqlKeyword.Def.WITH.value, CqlCompletion.Builder.naturalOrder().all(CqlKeyword.Def.VALUES.value).all
                (CqlKeyword.Def.AND.value).all(CqlKeyword.Def.REPLICATION.value).all(CqlKeyword.Def.DURABLE_WRITES
                .value).prefix(CqlKeyword.Def.CLASS.value).prefix(CqlKeyword.Def.SIMPLE_STRATEGY.value).prefix(CqlKeyword.Def.REPLICATION_FACTOR.value).prefix(CqlKeyword.Def.NETWORK_TOPOLOGY_STRATEGY.value).prefix(CqlKeyword.Def.DURABLE_WRITES.value).all(CqlKeyword.Def.TRUE.value).all(CqlKeyword.Def.FALSE.value)
                .prefix(CqlKeyword.Def.OLD_NETWORK_TOPOLOGY_STRATEGY.value).build());
    }

    @Override
    public String toString() {
        return Objects.toStringHelper(this).toString();
    }

}

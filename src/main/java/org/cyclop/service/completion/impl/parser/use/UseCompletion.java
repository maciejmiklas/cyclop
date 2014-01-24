package org.cyclop.service.completion.impl.parser.use;

import com.google.common.base.Objects;
import com.google.common.collect.ImmutableSortedSet;
import javax.inject.Inject;
import javax.inject.Named;
import org.cyclop.model.CqlCompletion;
import org.cyclop.model.CqlKeySpace;
import org.cyclop.model.CqlKeyword;
import org.cyclop.model.CqlQuery;
import org.cyclop.service.cassandra.QueryService;
import org.cyclop.service.completion.impl.parser.MarkerBasedCompletion;

/**
 * @author Maciej Miklas
 */
@Named("use.UsePartCompletion")
class UseCompletion extends MarkerBasedCompletion {

    @Inject
    private QueryService queryService;

    public UseCompletion() {
        super(CqlKeyword.Def.USE.value);
    }

    @Override
    public CqlCompletion getCompletion(CqlQuery query) {
        ImmutableSortedSet<CqlKeySpace> keySpaces = queryService.findAllKeySpaces();
        return CqlCompletion.Builder.naturalOrder().all(keySpaces).build();
    }

    @Override
    public String toString() {
        return Objects.toStringHelper(this).toString();
    }

}

package org.cyclop.service.completion.parser.dropkeyspace;

import com.google.common.collect.ImmutableSortedSet;
import javax.annotation.PostConstruct;
import javax.inject.Inject;
import javax.inject.Named;
import org.cyclop.model.CqlCompletion;
import org.cyclop.model.CqlKeySpace;
import org.cyclop.model.CqlKeyword;
import org.cyclop.model.CqlQuery;
import org.cyclop.service.cassandra.QueryService;
import org.cyclop.service.completion.parser.MarkerBasedCompletion;

/**
 * @author Maciej Miklas
 */
@Named("dropkeyspace.DropCompletion")
public class DropCompletion extends MarkerBasedCompletion {

    @Inject
    private QueryService queryService;

    private CqlCompletion.BuilderTemplate completion;

    @PostConstruct
    public void init() {
        completion = CqlCompletion.Builder.naturalOrder().all(CqlKeyword.Def.IF_EXISTS.value).template();
    }

    public DropCompletion() {
        super(CqlKeyword.Def.DROP_KEYSPACE.value);
    }

    @Override
    public CqlCompletion getCompletion(CqlQuery query) {
        ImmutableSortedSet<CqlKeySpace> keySpaces = queryService.findAllKeySpaces();
        return completion.naturalOrder().all(keySpaces).build();
    }

}

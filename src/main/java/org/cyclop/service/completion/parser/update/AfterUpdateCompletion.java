package org.cyclop.service.completion.parser.update;

import com.google.common.collect.ImmutableSortedSet;
import org.cyclop.common.QueryHelper;
import org.cyclop.model.*;
import org.cyclop.service.cassandra.QueryService;
import org.cyclop.service.completion.parser.CqlPartCompletionDynamic;
import org.cyclop.service.completion.parser.template.AfterTableNameCompletionTemplate;

import javax.annotation.PostConstruct;
import javax.inject.Inject;
import javax.inject.Named;

import static org.cyclop.model.CqlKeywords.UPDATE;

/**
 * @author Maciej Miklas
 */
@Named("update.AfterUpdateCompletion")
public class AfterUpdateCompletion extends AfterTableNameCompletionTemplate {

    private CqlCompletion completion;

    @Inject
    private QueryService queryService;

    public AfterUpdateCompletion() {
        super(UPDATE);
    }

    @PostConstruct
    public void init() {
        ImmutableSortedSet.Builder<CqlPart> completionBuild = ImmutableSortedSet.naturalOrder();
        completionBuild.add(new CqlKeyword("using ttl"));
        completionBuild.add(new CqlKeyword("using timestamp"));
        completionBuild.add(new CqlKeyword("set"));
        ImmutableSortedSet<CqlPart> staticPart = completionBuild.build();
        completion = new CqlCompletion(staticPart, staticPart);
    }

    @Override
    public CqlCompletion getCompletion(CqlQuery query) {
        return completion;
    }

}

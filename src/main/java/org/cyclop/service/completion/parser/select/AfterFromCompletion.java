package org.cyclop.service.completion.parser.select;

import com.google.common.collect.ImmutableSortedSet;
import org.cyclop.common.QueryHelper;
import org.cyclop.model.*;
import org.cyclop.service.cassandra.QueryService;
import org.cyclop.service.completion.parser.CqlPartCompletionDynamic;
import org.cyclop.service.completion.parser.template.AfterTableNameCompletionTemplate;

import javax.annotation.PostConstruct;
import javax.inject.Inject;
import javax.inject.Named;

import static org.cyclop.model.CqlKeywords.FROM;

/**
 * @author Maciej Miklas
 */
@Named("select.AfterFromCompletion")
public class AfterFromCompletion extends AfterTableNameCompletionTemplate {

    private CqlCompletion completion;

    @Inject
    private QueryService queryService;

    public AfterFromCompletion() {
        super(FROM);
    }

    @PostConstruct
    public void init() {
        completion = CqlCompletion.Builder.naturalOrder().all(new CqlKeyword("where")).all(new CqlKeyword("order by")).
                all(new CqlKeyword("limit")).all(new CqlKeyword("allow filtering")).build();
    }

    @Override
    public CqlCompletion getCompletion(CqlQuery query) {
        return completion;
    }
}

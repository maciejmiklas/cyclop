package org.cyclop.service.completion.parser.update;

import org.cyclop.model.CqlCompletion;
import org.cyclop.model.CqlKeyword;
import org.cyclop.model.CqlQuery;
import org.cyclop.service.cassandra.QueryService;
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
        completion = CqlCompletion.Builder.naturalOrder().all(new CqlKeyword("using ttl")).all(new CqlKeyword("using timestamp")).
                all(new CqlKeyword("set")).build();
    }

    @Override
    public CqlCompletion getCompletion(CqlQuery query) {
        return completion;
    }

}

package org.cyclop.service.completion.parser.delete;

import org.cyclop.model.CqlCompletion;
import org.cyclop.model.CqlKeyword;
import org.cyclop.model.CqlQuery;
import org.cyclop.service.cassandra.QueryService;
import org.cyclop.service.completion.parser.template.AfterTableNameCompletionTemplate;

import javax.annotation.PostConstruct;
import javax.inject.Inject;
import javax.inject.Named;

import static org.cyclop.model.CqlKeywords.FROM;

/**
 * @author Maciej Miklas
 */
@Named("delete.AfterFromCompletion")
public class AfterFromCompletion extends AfterTableNameCompletionTemplate {

    private CqlCompletion completion;

    @Inject
    private QueryService queryService;

    public AfterFromCompletion() {
        super(FROM);
    }

    @PostConstruct
    public void init() {
        completion = CqlCompletion.Builder.naturalOrder().all(new CqlKeyword("where")).
                all(new CqlKeyword("using timestamp")).build();
    }

    @Override
    public CqlCompletion getCompletion(CqlQuery query) {
        return completion;
    }

}

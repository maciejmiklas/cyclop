package org.cyclop.service.completion.parser;

import com.google.common.collect.ImmutableSortedSet;
import org.cyclop.model.CqlCompletion;
import org.cyclop.model.CqlNotSupported;
import org.cyclop.model.CqlPart;
import org.cyclop.model.CqlQuery;

import javax.annotation.PostConstruct;
import javax.inject.Named;

/**
 * @author Maciej Miklas
 */
@Named
public abstract class NotSupportedCompletion implements CqlPartCompletionStatic {

    private CqlCompletion completion;

    @PostConstruct
    public void init() {
        completion = CqlCompletion.Builder.naturalOrder().all(new CqlNotSupported("'" + getNotSupportedText() + "' is not supported yet.....")).build();
    }

    @Override
    public final CqlCompletion getCompletion(CqlQuery query) {
        return completion;
    }

    protected abstract String getNotSupportedText();

}

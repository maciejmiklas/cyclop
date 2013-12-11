package org.cyclop.service.completion.parser.decisionlist;

import com.google.common.collect.ImmutableSortedSet;
import javax.annotation.PostConstruct;
import javax.inject.Named;
import org.cyclop.service.model.CqlCompletion;
import org.cyclop.service.model.CqlNotSupported;
import org.cyclop.service.model.CqlPart;
import org.cyclop.service.model.CqlQuery;

/**
 * @author Maciej Miklas
 */
@Named
public abstract class NotSupportedCompletion implements CqlPartCompletionStatic {

    private CqlCompletion completion;

    @PostConstruct
    public void init() {
        ImmutableSortedSet.Builder<CqlPart> completionBuild = ImmutableSortedSet.naturalOrder();
        completionBuild.add(new CqlNotSupported("'" + getNotSupportedText() + "' is not supported yet....."));
        ImmutableSortedSet<CqlPart> staticPart = completionBuild.build();
        completion = new CqlCompletion(staticPart, staticPart);
    }

    @Override
    public final CqlCompletion getCompletion(CqlQuery query) {
        return completion;
    }

    protected abstract String getNotSupportedText();

}

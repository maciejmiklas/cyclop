package org.cyclop.service.completion.parser.decisionlist.insert;

import com.google.common.collect.ImmutableSortedSet;
import org.cyclop.model.CqlCompletion;
import org.cyclop.model.CqlKeyword;
import org.cyclop.model.CqlPart;
import org.cyclop.model.CqlQuery;
import org.cyclop.service.completion.parser.decisionlist.CqlPartCompletionStatic;

import javax.annotation.PostConstruct;
import javax.inject.Named;

/**
 * @author Maciej Miklas
 */
@Named("insert.AfterValuesPartCompletion")
public class AfterValuesPartCompletion implements CqlPartCompletionStatic {

    private final CqlPart[] startMarker = new CqlPart[]{new CqlPart(")")};

    private CqlCompletion cmpMinMax = null;

    @PostConstruct
    public void init() {

        ImmutableSortedSet.Builder<CqlPart> comContentBuild = ImmutableSortedSet.naturalOrder();
        comContentBuild.add(new CqlKeyword("using ttl"));
        comContentBuild.add(new CqlKeyword("using timestamp"));
        comContentBuild.add(new CqlKeyword("and"));
        ImmutableSortedSet<CqlPart> compContent = comContentBuild.build();
        cmpMinMax = new CqlCompletion(compContent, compContent);
    }

    @Override
    public CqlCompletion getCompletion(CqlQuery query) {
        return cmpMinMax;
    }

    @Override
    public CqlPart[] startMarkers() {
        return startMarker;
    }

}


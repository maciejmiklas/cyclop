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
@Named("insert.AfterColumnsCompletion")
public class AfterColumnsCompletion implements CqlPartCompletionStatic {


    private CqlCompletion cmpMinMax;

    private final CqlPart[] startMarker = new CqlPart[]{new CqlPart(")")};


    @PostConstruct
    public void init() {
        ImmutableSortedSet.Builder<CqlPart> comContentBuild = ImmutableSortedSet.naturalOrder();
        comContentBuild.add(new CqlKeyword("values"));
        ImmutableSortedSet<CqlPart> comContent = comContentBuild.build();
        cmpMinMax = new CqlCompletion(comContent, comContent);
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

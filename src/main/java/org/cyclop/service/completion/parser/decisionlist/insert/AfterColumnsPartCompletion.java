package org.cyclop.service.completion.parser.decisionlist.insert;

import com.google.common.collect.ImmutableSortedSet;
import javax.annotation.PostConstruct;
import javax.inject.Named;
import org.cyclop.service.completion.parser.decisionlist.CqlPartCompletionStatic;
import org.cyclop.service.model.CqlCompletion;
import org.cyclop.service.model.CqlKeyword;
import org.cyclop.service.model.CqlPart;
import org.cyclop.service.model.CqlQuery;

/**
 * @author Maciej Miklas
 */
@Named("insert.AfterColumnsPartCompletion")
public class AfterColumnsPartCompletion implements CqlPartCompletionStatic {


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

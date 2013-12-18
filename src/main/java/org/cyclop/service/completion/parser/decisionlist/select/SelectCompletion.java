package org.cyclop.service.completion.parser.decisionlist.select;

import org.cyclop.model.CqlKeyword;
import org.cyclop.model.CqlPart;
import org.cyclop.service.cassandra.CassandraSession;
import org.cyclop.service.cassandra.QueryService;
import org.cyclop.service.completion.parser.decisionlist.common.TableNameCompletion;

import javax.inject.Inject;
import javax.inject.Named;
import java.util.ArrayList;
import java.util.List;

import static org.cyclop.model.CqlKeywords.FROM;

/**
 * @author Maciej Miklas
 */
@Named("select.SelectCompletion")
public class SelectCompletion extends TableNameCompletion {

    @Inject
    private QueryService queryService;

    private static final CqlPart[] startMarker = new CqlPart[]{new CqlKeyword("select")};

    private final static List<CqlPart> staticPart = new ArrayList<>();

    @Inject
    private CassandraSession session;

    public SelectCompletion() {
        super(staticPart, startMarker, FROM);
    }

    static {
        staticPart.add(new CqlKeyword("count (*)"));
        staticPart.add(new CqlKeyword("count (1)"));
        staticPart.add(new CqlKeyword("writetime"));
        staticPart.add(new CqlKeyword("ttl"));
        staticPart.add(new CqlKeyword("from"));
    }

}

package org.cyclop.service.completion;

import com.google.common.collect.ImmutableSortedSet;
import org.cyclop.model.*;
import org.cyclop.service.cassandra.QueryService;
import org.cyclop.test.AbstractTestCase;
import org.cyclop.test.ValidationHelper;
import org.junit.Test;

import javax.inject.Inject;

import static junit.framework.Assert.assertTrue;

/**
 * @author Maciej Miklas
 */
public class TestCompletionService extends AbstractTestCase {

    @Inject
    private QueryService qs;

    @Inject
    private ValidationHelper vh;

    @Inject
    private CompletionService cs;

    @Test
    public void testFindInitialCompletion() {
        ContextCqlCompletion completion = cs.findInitialCompletion();

        vh.verifyFullAndMinCompletionTheSame(completion, 15);

        ImmutableSortedSet<? extends CqlPart> cmp = completion.cqlCompletion.fullCompletion;
        assertTrue(cmp.toString(), cmp.contains(CqlKeyword.Def.USE.value));
        assertTrue(cmp.toString(), cmp.contains(CqlKeyword.Def.UPDATE.value));
        assertTrue(cmp.toString(), cmp.contains(CqlKeyword.Def.TRUNCATE.value));
        assertTrue(cmp.toString(), cmp.contains(CqlKeyword.Def.SELECT.value));
        assertTrue(cmp.toString(), cmp.contains(CqlKeyword.Def.INSERT_INTO.value));
        assertTrue(cmp.toString(), cmp.contains(CqlKeyword.Def.DROP_TABLE.value));
        assertTrue(cmp.toString(), cmp.contains(CqlKeyword.Def.DROP_KEYSPACE.value));
        assertTrue(cmp.toString(), cmp.contains(CqlKeyword.Def.DROP_INDEX.value));
        assertTrue(cmp.toString(), cmp.contains(CqlKeyword.Def.DELETE.value));
    }

    @Test
    public void testFindCompletion_Select_SelectContainsAllColumns() {
        ContextCqlCompletion completion = cs.findCompletion(new CqlQuery(CqlQueryName.SELECT, "select *"), 8);
        vh.verifyFullAndMinCompletionTheSame(completion, 80);
        ImmutableSortedSet<? extends CqlPart> cmp = completion.cqlCompletion.fullCompletion;

        vh.verifyMybooksColumns(cmp, true);
        vh.verifyFromKeywords(cmp, true);
        vh.verifySystemColumns(cmp, true);
        vh.verifyCompoundTestColumns(cmp, true);
    }

    @Test
    public void testFindCompletion_Select_ContainsColumnsFromKeyspaceWithTable_CursorOnEnd() {
        ContextCqlCompletion completion = cs.findCompletion(new CqlQuery(CqlQueryName.SELECT, "select * from cqldemo.mybooks"), 8);
        vh.verifyColumnsFromMybooksAndFrom(completion);
    }

    @Test
    public void testFindCompletion_Select_ContainsColumnsFromKeyspaceWithTable_CursonOnFr() {
        ContextCqlCompletion completion = cs.findCompletion(new CqlQuery(CqlQueryName.SELECT, "select * from cqldemo.mybooks"), 10);
        vh.verifyColumnsFromMybooksAndFrom(completion);
    }

    @Test
    public void testFindCompletion_Select_ContainsColumnsFromKeyspaceInSessionScope() {
        qs.execute(new CqlQuery(CqlQueryName.USE, "use cqldemo"));
        ContextCqlCompletion completion = cs.findCompletion(new CqlQuery(CqlQueryName.SELECT, "select * from mybooks"), 8);
        vh.verifyColumnsFromMybooksAndFrom(completion);
    }

    @Test
    public void testFindCompletion_Select_TablesFromCqldemoSpace() {
        qs.execute(new CqlQuery(CqlQueryName.USE, "use cqldemo"));
        ContextCqlCompletion completion = cs.findCompletion(new CqlQuery(CqlQueryName.SELECT, "select * from cqldemo."), 22);
        vh.verifyFullAndMinCompletionNotTheSame(completion, 2, 4);

        ImmutableSortedSet<? extends CqlPart> fullCompletion = completion.cqlCompletion.fullCompletion;
        vh.verifyTableNamesCqlDemo(fullCompletion, true);
        vh.verifyTableNamesSystem(fullCompletion, false);
        vh.verifyTableNamesWithSpaceCqlDemo(fullCompletion, true);


        ImmutableSortedSet<? extends CqlPart> minCompletion = completion.cqlCompletion.minCompletion;
        vh.verifyTableNamesCqlDemo(minCompletion, true);
        vh.verifyTableNamesSystem(minCompletion, false);
        vh.verifyTableNamesWithSpaceCqlDemo(minCompletion, false);
    }

    @Test
    public void testFindCompletion_Select_SelectTableFromDifferentSpace() {
        qs.execute(new CqlQuery(CqlQueryName.USE, "use system"));
        ContextCqlCompletion completion = cs.findCompletion(new CqlQuery(CqlQueryName.SELECT, "select * from mybooks"), 8);
        vh.verifyColumnsFromMybooksAndFrom(completion);
    }

}

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
        vh.verifyFullAndMinCompletionTheSame(completion, 30);
        ImmutableSortedSet<? extends CqlPart> cmp = completion.cqlCompletion.fullCompletion;

        vh.verifyMybooksColumns(cmp, true);
        vh.verifyFromKeywords(cmp, true);
        vh.verifySystemColumns(cmp, true);
        vh.verifyCompoundTestColumns(cmp, true);
    }

    @Test
    public void testFindCompletion_Select_AfterTableName() {
        ContextCqlCompletion completion = cs.findCompletion(new CqlQuery(CqlQueryName.SELECT,
                "select * from cqldemo.mybooks "), 30);
        vh.verifyFullAndMinCompletionTheSame(completion, 4);

        ImmutableSortedSet<? extends CqlPart> cmp = completion.cqlCompletion.fullCompletion;
        vh.verifyMybooksColumns(cmp, false);
        assertTrue(cmp.toString(), cmp.contains(CqlKeyword.Def.WHERE.value));
        assertTrue(cmp.toString(), cmp.contains(CqlKeyword.Def.LIMIT.value));
        assertTrue(cmp.toString(), cmp.contains(CqlKeyword.Def.ORDER_BY.value));
        assertTrue(cmp.toString(), cmp.contains(CqlKeyword.Def.ALLOW_FILTERING.value));
    }

    @Test
    public void testFindCompletion_Select_AfterWhere() {
        ContextCqlCompletion completion = cs.findCompletion(new CqlQuery(CqlQueryName.SELECT,
                "select * from cqldemo.mybooks  where "), 37);
        vh.verifyFullAndMinCompletionNotTheSame(completion, 18, 18);

        ImmutableSortedSet<? extends CqlPart> fcmp = completion.cqlCompletion.fullCompletion;
        ImmutableSortedSet<? extends CqlPart> mcmp = completion.cqlCompletion.minCompletion;
        vh.verifyMybooksColumns(fcmp, true);
        vh.verifyKeyspaces(fcmp, false);
        vh.verifyTableNamesSystem(fcmp, false);
        vh.verifyCompoundTestColumns(fcmp, false);

        vh.verifyContainsOnlyKeywords(fcmp, CqlKeyword.Def.ORDER_BY, CqlKeyword.Def.LIMIT, CqlKeyword.Def.IN_BL,
                CqlKeyword.Def.AND, CqlKeyword.Def.ALLOW_FILTERING);

        vh.verifyContainsOnlyKeywords(mcmp, CqlKeyword.Def.ORDER_BY, CqlKeyword.Def.LIMIT, CqlKeyword.Def.IN,
                CqlKeyword.Def.AND, CqlKeyword.Def.ALLOW_FILTERING);
    }

    @Test
    public void testFindCompletion_Select_AfterOrderBy() {
        ContextCqlCompletion completion = cs.findCompletion(new CqlQuery(CqlQueryName.SELECT,
                "select * from cqldemo.mybooks  where king " +
                "" + "= 'none' AND reign_start >= 1500 AND reign_start < 3000 LIMIT 10 ALLOW FILTERING ORDER by "), 131);
        vh.verifyFullAndMinCompletionTheSame(completion, 18);

        ImmutableSortedSet<? extends CqlPart> cmp = completion.cqlCompletion.fullCompletion;
        vh.verifyMybooksColumns(cmp, true);
        vh.verifyKeyspaces(cmp, false);
        vh.verifyTableNamesSystem(cmp, false);
        vh.verifyCompoundTestColumns(cmp, false);

        vh.verifyContainsOnlyKeywords(cmp, CqlKeyword.Def.TOKEN, CqlKeyword.Def.LIMIT, CqlKeyword.Def.DESC, CqlKeyword.Def.ASC,
                CqlKeyword.Def.ALLOW_FILTERING);
    }

    @Test
    public void testFindCompletion_Select_ContainsColumnsFromKeyspaceWithTable_CursorOnEnd() {
        ContextCqlCompletion completion = cs.findCompletion(new CqlQuery(CqlQueryName.SELECT, "select * from cqldemo.mybooks"),
                8);
        vh.verifyColumnsFromMybooksAndFrom(completion);
    }

    @Test
    public void testFindCompletion_Select_ContainsColumnsFromKeyspaceWithTable_CursorOnFrom() {
        ContextCqlCompletion completion = cs.findCompletion(new CqlQuery(CqlQueryName.SELECT, "select * from cqldemo.mybooks"),
                10);
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

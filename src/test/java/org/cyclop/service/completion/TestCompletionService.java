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
    public void testFindCompletion_CreateKeyspace_AfterCreate() throws Exception {
        ContextCqlCompletion completion = cs.findCompletion(new CqlQuery(CqlQueryName.CREATE_KEYSPACE, "create keyspace "));

        vh.verifyFullAndMinCompletionTheSame(completion, 2);

        ImmutableSortedSet<? extends CqlPart> cmp = completion.cqlCompletion.fullCompletion;
        vh.verifyContainsOnlyKeywords(cmp, CqlKeyword.Def.WITH, CqlKeyword.Def.IF_NOT_EXISTS);
    }

    @Test
    public void testFindCompletion_CreateKeyspace_AfterWith() throws Exception {
        verifyCreateKeyspaceWith("create keyspace with ");
    }

    @Test
    public void testFindCompletion_CreateKeyspace_ExistsWith() throws Exception {
        verifyCreateKeyspaceWith("create keyspace inf not exists with ");
    }

    @Test
    public void testFindCompletion_Delete_AfterDelete() throws Exception {
        ContextCqlCompletion completion = cs.findCompletion(new CqlQuery(CqlQueryName.DELETE, "delete "));

        vh.verifyFullAndMinCompletionTheSame(completion, 30);
        ImmutableSortedSet<? extends CqlPart> cmp = completion.cqlCompletion.fullCompletion;

        vh.verifyMybooksColumns(cmp, true);
        vh.verifySystemColumns(cmp, true);
        vh.verifyCompoundTestColumns(cmp, true);
        vh.verifyContainsOnlyKeywords(cmp, CqlKeyword.Def.FROM);
    }

    @Test
    public void testFindCompletion_Deletet_AfterFrom_CqldemoSpaceInQuery() throws Exception {
        qs.execute(new CqlQuery(CqlQueryName.USE, "use system"));

        ContextCqlCompletion completion = cs.findCompletion(new CqlQuery(CqlQueryName.DELETE, "delete abc from cqldemo."));

        verifyAfterFromCqldemoSpaceInQuery(completion);
    }

    @Test
    public void testFindCompletion_Deletet_AfterFrom_NoSpqceInQuery() throws Exception {
        qs.execute(new CqlQuery(CqlQueryName.USE, "use cqldemo"));

        ContextCqlCompletion completion = cs.findCompletion(new CqlQuery(CqlQueryName.DELETE, "delete abc from "));

        verifyAfterFromNoSpqceInQuery(completion);
    }

    @Test
    public void testFindCompletion_Select_AfterFrom_CqldemoSpaceInQuery() {
        qs.execute(new CqlQuery(CqlQueryName.USE, "use system"));
        ContextCqlCompletion completion = cs.findCompletion(new CqlQuery(CqlQueryName.SELECT, "select * from cqldemo."));
        verifyAfterFromCqldemoSpaceInQuery(completion);
    }

    @Test
    public void testFindCompletion_Select_AfterFrom_NoSpqceInQuery() {
        qs.execute(new CqlQuery(CqlQueryName.USE, "use cqldemo"));
        ContextCqlCompletion completion = cs.findCompletion(new CqlQuery(CqlQueryName.SELECT, "select * from "));
        verifyAfterFromNoSpqceInQuery(completion);
    }

    @Test
    public void testFindCompletion_Select_AfterOrderBy() {
        veifySelectWithOrderBy("select * from cqldemo.mybooks  where king = 'none' AND reign_start >= 1500 AND reign_start < "
                + "3000 LIMIT 10 ALLOW FILTERING ORDER by ");
    }

    @Test
    public void testFindCompletion_Select_AfterOrderBy_NoWhere() {
        veifySelectWithOrderBy("select * from cqldemo.mybooks ORDER by ");
    }

    @Test
    public void testFindCompletion_Select_AfterSelect_ContainsAllColumns() {
        ContextCqlCompletion completion = cs.findCompletion(new CqlQuery(CqlQueryName.SELECT, "select *"), 88);
        vh.verifyFullAndMinCompletionTheSame(completion, 30);
        ImmutableSortedSet<? extends CqlPart> cmp = completion.cqlCompletion.fullCompletion;

        vh.verifyMybooksColumns(cmp, true);
        vh.verifyFromKeywords(cmp, true);
        vh.verifySystemColumns(cmp, true);
        vh.verifyCompoundTestColumns(cmp, true);
    }

    @Test
    public void testFindCompletion_Select_AfterSelect_ContainsColumnsFromKeyspaceInSessionScope() {
        qs.execute(new CqlQuery(CqlQueryName.USE, "use cqldemo"));
        ContextCqlCompletion completion = cs.findCompletion(new CqlQuery(CqlQueryName.SELECT, "select * from mybooks"), 8);
        verifyColumnsFromMybooksAndFrom(completion);
    }

    @Test
    public void testFindCompletion_Select_AfterSelect_ContainsColumnsFromKeyspaceWithTable_CursorOnEnd() {
        ContextCqlCompletion completion = cs.findCompletion(new CqlQuery(CqlQueryName.SELECT, "select * from cqldemo.mybooks"),
                8);
        verifyColumnsFromMybooksAndFrom(completion);
    }

    @Test
    public void testFindCompletion_Select_AfterSelect_ContainsColumnsFromKeyspaceWithTable_CursorOnFrom() {
        ContextCqlCompletion completion = cs.findCompletion(new CqlQuery(CqlQueryName.SELECT, "select * from cqldemo.mybooks"),
                10);
        verifyColumnsFromMybooksAndFrom(completion);
    }

    @Test
    public void testFindCompletion_Select_AfterSelect_SelectTableFromDifferentSpace() {
        qs.execute(new CqlQuery(CqlQueryName.USE, "use system"));
        ContextCqlCompletion completion = cs.findCompletion(new CqlQuery(CqlQueryName.SELECT, "select * from mybooks"), 8);
        verifyColumnsFromMybooksAndFrom(completion);
    }

    @Test
    public void testFindCompletion_Select_AfterTableName() {
        ContextCqlCompletion completion = cs.findCompletion(new CqlQuery(CqlQueryName.SELECT,
                "select * from cqldemo.mybooks "), 30);
        vh.verifyFullAndMinCompletionTheSame(completion, 4);

        ImmutableSortedSet<? extends CqlPart> cmp = completion.cqlCompletion.fullCompletion;
        vh.verifyMybooksColumns(cmp, false);

        vh.verifyContainsOnlyKeywords(cmp, CqlKeyword.Def.WHERE, CqlKeyword.Def.LIMIT, CqlKeyword.Def.ORDER_BY,
                CqlKeyword.Def.ALLOW_FILTERING);
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
    public void testFindInitialCompletion() {
        ContextCqlCompletion completion = cs.findInitialCompletion();

        vh.verifyFullAndMinCompletionTheSame(completion, 15);

        ImmutableSortedSet<? extends CqlPart> cmp = completion.cqlCompletion.fullCompletion;
        vh.verifyContainsOnlyKeywords(cmp, CqlKeyword.Def.USE, CqlKeyword.Def.UPDATE, CqlKeyword.Def.TRUNCATE,
                CqlKeyword.Def.SELECT, CqlKeyword.Def.INSERT_INTO, CqlKeyword.Def.DROP_TABLE, CqlKeyword.Def.DROP_KEYSPACE,
                CqlKeyword.Def.DROP_INDEX, CqlKeyword.Def.DELETE, CqlKeyword.Def.CREATE_KEYSPACE);
    }

    private void veifySelectWithOrderBy(String cql) {
        ContextCqlCompletion completion = cs.findCompletion(new CqlQuery(CqlQueryName.SELECT, cql), -1);
        vh.verifyFullAndMinCompletionTheSame(completion, 18);

        ImmutableSortedSet<? extends CqlPart> cmp = completion.cqlCompletion.fullCompletion;
        vh.verifyMybooksColumns(cmp, true);
        vh.verifyKeyspaces(cmp, false);
        vh.verifyTableNamesSystem(cmp, false);
        vh.verifyCompoundTestColumns(cmp, false);

        vh.verifyContainsOnlyKeywords(cmp, CqlKeyword.Def.TOKEN, CqlKeyword.Def.LIMIT, CqlKeyword.Def.DESC, CqlKeyword.Def.ASC,
                CqlKeyword.Def.ALLOW_FILTERING);
    }

    private void verifyAfterFromCqldemoSpaceInQuery(ContextCqlCompletion completion) {
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

    private void verifyAfterFromNoSpqceInQuery(ContextCqlCompletion completion) {
        vh.verifyFullAndMinCompletionNotTheSame(completion, 2, 4);

        ImmutableSortedSet<? extends CqlPart> minCompletion = completion.cqlCompletion.minCompletion;
        vh.verifyTableNamesCqlDemo(minCompletion, true);
        vh.verifyTableNamesSystem(minCompletion, false);
        vh.verifyTableNamesWithSpaceCqlDemo(minCompletion, false);
        vh.verifyKeyspaces(minCompletion, true);

        ImmutableSortedSet<? extends CqlPart> fullCompletion = completion.cqlCompletion.fullCompletion;
        vh.verifyTableNamesCqlDemo(fullCompletion, true);
        vh.verifyTableNamesSystem(fullCompletion, false);
        vh.verifyTableNamesWithSpaceCqlDemo(fullCompletion, false);

    }

    private void verifyColumnsFromMybooksAndFrom(ContextCqlCompletion completion) {
        vh.verifyFullAndMinCompletionTheSame(completion, 10);
        ImmutableSortedSet<? extends CqlPart> cmp = completion.cqlCompletion.fullCompletion;

        vh.verifyMybooksColumns(cmp, true);
        vh.verifyFromKeywords(cmp, true);
        vh.verifySystemColumns(cmp, false);
        vh.verifyCompoundTestColumns(cmp, false);
    }

    private void verifyCreateKeyspaceWith(String cql) {
        ContextCqlCompletion completion = cs.findCompletion(new CqlQuery(CqlQueryName.CREATE_KEYSPACE, cql));

        vh.verifyFullAndMinCompletionNotTheSame(completion, 11, 35);

        ImmutableSortedSet<? extends CqlPart> mcmp = completion.cqlCompletion.minCompletion;
        vh.verifyContainsOnlyKeywords(mcmp, CqlKeyword.Def.VALUES, CqlKeyword.Def.TRUE, CqlKeyword.Def.SIMPLE_STRATEGY,
                CqlKeyword.Def.REPLICATION_FACTOR, CqlKeyword.Def.REPLICATION, CqlKeyword.Def.OLD_NETWORK_TOPOLOGY_STRATEGY,
                CqlKeyword.Def.NETWORK_TOPOLOGY_STRATEGY, CqlKeyword.Def.FALSE, CqlKeyword.Def.DURABLE_WRITES,
                CqlKeyword.Def.CLASS, CqlKeyword.Def.AND);

        ImmutableSortedSet<? extends CqlPart> fcmp = completion.cqlCompletion.fullCompletion;
        assertTrue(fcmp.containsAll(mcmp));

        assertTrue(fcmp.toString(), fcmp.contains(new CqlPart(",oldnetworktopologystrategy")));
        assertTrue(fcmp.toString(), fcmp.contains(new CqlPart("(class")));
        assertTrue(fcmp.toString(), fcmp.contains(new CqlPart(":replication_factor")));
        assertTrue(fcmp.toString(), fcmp.contains(new CqlPart(":class")));
    }

}

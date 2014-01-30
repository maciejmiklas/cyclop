package org.cyclop.test;

import com.google.common.collect.ImmutableSortedSet;
import org.cyclop.model.*;

import javax.inject.Named;
import java.util.Collection;

import static junit.framework.Assert.*;

/**
 * @author Maciej Miklas
 */
@Named
public class ValidationHelper {

    public void verifyEmpty(Collection<?> col) {
        assertNotNull(col);
        assertEquals(0, col.size());
    }

    public void verifyColumnsFromMybooksAndFrom(ContextCqlCompletion completion) {
        verifyFullAndMinCompletionTheSame(completion, 10);
        ImmutableSortedSet<? extends CqlPart> cmp = completion.cqlCompletion.fullCompletion;

        verifyMybooksColumns(cmp, true);
        verifyFromKeywords(cmp, true);
        verifySystemColumns(cmp, false);
        verifyCompoundTestColumns(cmp, false);
    }

    public void verifyEmpty(ContextCqlCompletion completion) {
        assertNotNull(completion);
        assertNotNull(completion.cqlCompletion);
        assertNotNull(completion.cqlCompletion);
        verifyEmpty(completion.cqlCompletion.fullCompletion);
        verifyEmpty(completion.cqlCompletion.minCompletion);
    }

    public void verifyTableNamesCqlDemo(Collection<? extends CqlPart> col, boolean contains) {
        assertNotNull(col);
        assertTrue("Size:" + col.size(), col.size() >= 2);

        assertEquals(col.toString(), contains, col.contains(new CqlTable("mybooks")));
        assertEquals(col.toString(), contains, col.contains(new CqlTable("compoundtest")));
    }


    public void verifyTableNamesWithSpaceCqlDemo(Collection<? extends CqlPart> col, boolean contains) {
        assertNotNull(col);
        assertTrue("Size:" + col.size(), col.size() >= 2);

        assertEquals(col.toString(), contains, col.contains(new CqlTable("cqldemo", "mybooks")));
        assertEquals(col.toString(), contains, col.contains(new CqlTable("cqldemo", "compoundtest")));
    }


    public void verifyTableNamesSystem(Collection<? extends CqlPart> col, boolean contains) {
        assertNotNull(col);
        assertTrue("Size:" + col.size(), col.size() >= (contains ? 10 : 0));

        assertEquals(col.toString(), contains, col.contains(new CqlTable("sstable_activity")));
        assertEquals(col.toString(), contains, col.contains(new CqlTable("schema_triggers")));
        assertEquals(col.toString(), contains, col.contains(new CqlTable("range_xfers")));
        assertEquals(col.toString(), contains, col.contains(new CqlTable("nodeidinfo")));
        assertEquals(col.toString(), contains, col.contains(new CqlTable("local")));
        assertEquals(col.toString(), contains, col.contains(new CqlTable("compactions_in_progress")));
        assertEquals(col.toString(), contains, col.contains(new CqlTable("compaction_history")));
    }

    public void verifyIndexFromCqlDemo(Collection<? extends CqlPart> col, boolean contains) {
        assertNotNull(col);
        assertTrue("Size:" + col.size(), col.size() >= 4);

        assertEquals(col.toString(), contains, col.contains(new CqlIndex("mybooks_publishdate_idx")));
        assertEquals(col.toString(), contains, col.contains(new CqlIndex("mybooks_pages_idx")));
        assertEquals(col.toString(), contains, col.contains(new CqlIndex("mybooks_genre_idx")));
    }

    public void verifyKeyspaces(Collection<? extends CqlPart> col, boolean contains) {
        assertNotNull(col);
        assertEquals(col.toString(), contains, col.contains(new CqlKeySpace("cqldemo")));
        assertEquals(col.toString(), contains, col.contains(new CqlKeySpace("system")));
    }

    public void verifyCompoundTestColumns(Collection<? extends CqlPart> col, boolean contains) {
        assertNotNull(col);
        assertEquals(col.toString(), contains, col.contains(new CqlColumnName("id2")));
        assertEquals(col.toString(), contains, col.contains(new CqlColumnName("id3")));
        assertEquals(col.toString(), contains, col.contains(new CqlColumnName("deesc")));
    }

    public void verifySystemColumns(Collection<? extends CqlPart> col, boolean contains) {
        assertNotNull(col);
        assertEquals(col.toString(), contains, col.contains(new CqlColumnName("truncated_at")));
        assertEquals(col.toString(), contains, col.contains(new CqlColumnName("tokens")));
        assertEquals(col.toString(), contains, col.contains(new CqlColumnName("peer")));
        assertEquals(col.toString(), contains, col.contains(new CqlColumnName("token_bytes")));
        assertEquals(col.toString(), contains, col.contains(new CqlColumnName("rpc_address")));
        assertEquals(col.toString(), contains, col.contains(new CqlColumnName("replicate_on_write")));
    }

    public void verifyMybooksColumns(Collection<? extends CqlPart> col, boolean contains) {
        assertNotNull(col);
        assertEquals(col.toString(), contains, col.contains(new CqlColumnName("title")));
        assertEquals(col.toString(), contains, col.contains(new CqlColumnName("price")));
        assertEquals(col.toString(), contains, col.contains(new CqlColumnName("genre")));
        assertEquals(col.toString(), contains, col.contains(new CqlColumnName("publishDate")));
        assertEquals(col.toString(), contains, col.contains(new CqlColumnName("description")));
        assertEquals(col.toString(), contains, col.contains(new CqlColumnName("dynamiccolumn1")));
        assertEquals(col.toString(), contains, col.contains(new CqlColumnName("dynamiccolumn2")));
        assertEquals(col.toString(), contains, col.contains(new CqlColumnName("dynamiccolumn3")));
    }

    public void verifyFromKeywords(Collection<? extends CqlPart> col, boolean contains) {
        assertNotNull(col);
        assertEquals(col.toString(), contains, col.contains(CqlKeyword.Def.FROM.value));
        assertEquals(col.toString(), contains, col.contains(CqlKeyword.Def.COUNT_AST.value));
        assertEquals(col.toString(), contains, col.contains(CqlKeyword.Def.COUNT_ONE.value));
    }

    public void verifyFullAndMinCompletionTheSame(ContextCqlCompletion completion, int minSize) {
        assertNotNull(completion);
        ImmutableSortedSet<? extends CqlPart> fullCompletion = completion.cqlCompletion.fullCompletion;
        assertNotNull(fullCompletion);
        ImmutableSortedSet<? extends CqlPart> minCompletion = completion.cqlCompletion.minCompletion;
        assertNotNull(minCompletion);

        assertEquals(fullCompletion.size(), minCompletion.size());
        assertTrue(fullCompletion + " - " + minCompletion, fullCompletion.containsAll(minCompletion));
        assertTrue(minCompletion.size() + ">" + minSize, minCompletion.size() >= minSize);
    }

    public void verifyFullAndMinCompletionNotTheSame(ContextCqlCompletion completion, int minMinSize, int fullMinSize) {
        assertNotNull(completion);
        ImmutableSortedSet<? extends CqlPart> fullCompletion = completion.cqlCompletion.fullCompletion;
        assertNotNull(fullCompletion);
        ImmutableSortedSet<? extends CqlPart> minCompletion = completion.cqlCompletion.minCompletion;
        assertNotNull(minCompletion);

        assertTrue(fullCompletion.size() != minCompletion.size());
        assertTrue(minCompletion.size() + ">" + minMinSize, minCompletion.size() >= minMinSize);
        assertTrue(fullCompletion.size() + ">" + fullMinSize, fullCompletion.size() >= fullMinSize);
    }
}

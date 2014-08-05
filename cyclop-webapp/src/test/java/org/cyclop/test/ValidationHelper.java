/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.cyclop.test;

import static junit.framework.Assert.assertEquals;
import static junit.framework.Assert.assertNotNull;
import static junit.framework.Assert.assertTrue;
import static org.junit.Assert.assertFalse;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import javax.inject.Named;

import org.cyclop.model.ContextCqlCompletion;
import org.cyclop.model.CqlColumnName;
import org.cyclop.model.CqlIndex;
import org.cyclop.model.CqlKeySpace;
import org.cyclop.model.CqlKeyword;
import org.cyclop.model.CqlPart;
import org.cyclop.model.CqlTable;

import com.google.common.collect.ImmutableSet;
import com.google.common.collect.ImmutableSortedSet;

/** @author Maciej Miklas */
@Named
public class ValidationHelper {

    public Collection<? extends CqlPart> asHahsCol(Collection<? extends CqlPart> col) {
	if (col == null) {
	    return null;
	}
	return ImmutableSet.copyOf(col);
    }

    public void verifyCompletionNotSupported(ContextCqlCompletion completion, String pref) {
	verifyFullAndMinCompletionTheSame(completion, 1);
	completion.cqlCompletion.fullCompletion.contains(new CqlPart(pref + "' is not supported yet....."));
    }

    public void verifyContainsAllKeyspacesAndTables(
	    ContextCqlCompletion completion,
	    boolean spaceCqlDemoOrSystem) {
	int limt = spaceCqlDemoOrSystem ? 5 : 19;
	verifyFullAndMinCompletionNotTheSame(completion, limt, limt);
	verifyContainsAllColumns(completion, false);

	{
	    Collection<? extends CqlPart> mcmp = asHahsCol(completion.cqlCompletion.minCompletion);
	    verifyContainsAllKeyspaces(mcmp, true);
	    verifyContainsNoKeywords(mcmp);
	    verifyContainsTableNamesCqlDemo(mcmp, spaceCqlDemoOrSystem);
	    verifyContainsTableNamesSystem(mcmp, !spaceCqlDemoOrSystem);
	    verifyContainsIndexFromCqlDemo(mcmp, false);
	    verifyContainsTableNamesWithSpaceCqlDemo(mcmp, false);
	}

	{
	    Collection<? extends CqlPart> fcmp = asHahsCol(completion.cqlCompletion.fullCompletion);
	    verifyContainsAllKeyspaces(fcmp, true, ".");
	    verifyContainsNoKeywords(fcmp);
	    verifyContainsTableNamesCqlDemo(fcmp, spaceCqlDemoOrSystem);
	    verifyContainsTableNamesSystem(fcmp, !spaceCqlDemoOrSystem);
	    verifyContainsIndexFromCqlDemo(fcmp, false);
	    verifyContainsTableNamesWithSpaceCqlDemo(fcmp, false);
	}
    }

    public void verifyCqldemoSpaceInQuery(ContextCqlCompletion completion) {
	verifyFullAndMinCompletionNotTheSame(completion, 2, 4);

	{
	    ImmutableSortedSet<? extends CqlPart> fcmp = completion.cqlCompletion.fullCompletion;
	    verifyContainsAllKeyspaces(fcmp, false);
	    verifyContainsTableNamesCqlDemo(fcmp, true);
	    verifyContainsTableNamesSystem(fcmp, false);
	    verifyContainsTableNamesWithSpaceCqlDemo(fcmp, true);
	}
	{
	    ImmutableSortedSet<? extends CqlPart> mcmp = completion.cqlCompletion.minCompletion;
	    verifyContainsAllKeyspaces(mcmp, false);
	    verifyContainsTableNamesCqlDemo(mcmp, true);
	    verifyContainsTableNamesSystem(mcmp, false);
	    verifyContainsTableNamesWithSpaceCqlDemo(mcmp, false);
	}
    }

    public void verifyContainsAllKeyspaces(Collection<? extends CqlPart> col, boolean contains) {
	verifyContainsAllKeyspaces(col, contains, "");
    }

    public void verifyContainsAllKeyspaces(Collection<? extends CqlPart> col, boolean contains, String postfix) {
	col = asHahsCol(col);
	assertNotNull(col);
	assertEquals(col.toString(), contains, col.contains(new CqlKeySpace("cqldemo" + postfix)));
	assertEquals(col.toString(), contains, col.contains(new CqlKeySpace("system" + postfix)));
    }

    public void verifyContainsAllColumns(ContextCqlCompletion completion, boolean all) {
	Collection<? extends CqlPart> cmp = asHahsCol(completion.cqlCompletion.fullCompletion);

	verifyContainsMybooksColumns(cmp, all);
	verifyContainsSystemColumns(cmp, all);
	verifyContainsCompoundTestColumns(cmp, all);
    }

    public void verifyContainsCompoundTestColumns(Collection<? extends CqlPart> col, boolean contains) {
	assertNotNull(col);
	assertEquals(col.toString(), contains, col.contains(new CqlColumnName("id2")));
	assertEquals(col.toString(), contains, col.contains(new CqlColumnName("id3")));
	assertEquals(col.toString(), contains, col.contains(new CqlColumnName("deesc")));
    }

    public void verifyContainsIndexFromCqlDemo(Collection<? extends CqlPart> col, boolean contains) {
	col = asHahsCol(col);
	assertNotNull(col);
	assertTrue("Size:" + col.size(), col.size() >= 4);

	assertEquals(col.toString(), contains, col.contains(new CqlIndex("mybooks_publishdate_idx")));
	assertEquals(col.toString(), contains, col.contains(new CqlIndex("mybooks_pages_idx")));
	assertEquals(col.toString(), contains, col.contains(new CqlIndex("mybooks_genre_idx")));
    }

    public void verifyContainsMybooksColumns(
	    Collection<? extends CqlPart> col,
	    boolean contains,
	    String... prefixes) {
	col = asHahsCol(col);
	assertNotNull(col);

	List<String> columnNamesBasic = new ArrayList<>();
	columnNamesBasic.add("title");
	columnNamesBasic.add("price");
	columnNamesBasic.add("genre");
	columnNamesBasic.add("publishDate");
	columnNamesBasic.add("description");
	columnNamesBasic.add("dynamiccolumn1");
	columnNamesBasic.add("dynamiccolumn2");
	columnNamesBasic.add("dynamiccolumn3");

	verifyContainsColumns(col, columnNamesBasic, contains, prefixes);
    }

    private void verifyContainsColumns(
	    Collection<? extends CqlPart> col,
	    List<String> columnNamesBasic,
	    boolean contains,
	    String... prefixes) {
	col = asHahsCol(col);
	assertNotNull(col);

	for (String colNam : columnNamesBasic) {
	    assertEquals(
		    colNam + " not found in: " + col.toString(),
		    contains,
		    col.contains(new CqlColumnName(colNam)));
	}

	for (String prefix : prefixes) {
	    for (String colNam : columnNamesBasic) {
		String conNamFull = prefix + colNam;
		assertEquals(
			conNamFull + " not found in: " + col.toString(),
			contains,
			col.contains(new CqlPart(conNamFull)));
	    }
	}

    }

    public void verifyContainsNoKeywords(Collection<? extends CqlPart> cmp) {
	for (CqlPart part : cmp) {
	    if (part instanceof CqlKeyword) {
		assertFalse(part + " FOUND in: " + cmp, true);
	    }
	}
    }

    public void verifyContainsOnlyKeywords(Collection<? extends CqlPart> cmp, CqlKeyword... keywords) {
	assertTrue(keywords.length > 0);
	Set<CqlKeyword> keywordsSet = new HashSet<>();

	for (CqlKeyword def : keywords) {
	    assertTrue(def + " not found in: " + cmp, cmp.contains(def));
	    keywordsSet.add(def);
	}

	for (CqlKeyword.Def def : CqlKeyword.Def.values()) {
	    if (keywordsSet.contains(def.value)) {
		continue;
	    }
	    assertFalse(def.value + " FOUND in: " + cmp, cmp.contains(def.value));
	}
    }

    public void verifyContainsSystemColumns(Collection<? extends CqlPart> col, boolean contains) {
	col = asHahsCol(col);
	assertNotNull(col);
	assertEquals(col.toString(), contains, col.contains(new CqlColumnName("truncated_at")));
	assertEquals(col.toString(), contains, col.contains(new CqlColumnName("tokens")));
	assertEquals(col.toString(), contains, col.contains(new CqlColumnName("peer")));
	assertEquals(col.toString(), contains, col.contains(new CqlColumnName("token_bytes")));
	assertEquals(col.toString(), contains, col.contains(new CqlColumnName("rpc_address")));
	assertEquals(col.toString(), contains, col.contains(new CqlColumnName("replicate_on_write")));
    }

    public void verifyContainsTableNamesCqlDemo(Collection<? extends CqlPart> col, boolean contains) {
	col = asHahsCol(col);
	assertNotNull(col);
	assertTrue("Size:" + col.size(), col.size() >= 2);

	assertEquals(col.toString(), contains, col.contains(new CqlTable("mybooks")));
	assertEquals(col.toString(), contains, col.contains(new CqlTable("compoundtest")));
    }

    public void verifyContainsTableNamesSystem(Collection<? extends CqlPart> col, boolean contains) {
	col = asHahsCol(col);
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

    public void verifyContainsTableNamesWithSpaceCqlDemo(Collection<? extends CqlPart> col, boolean contains) {
	col = asHahsCol(col);
	assertNotNull(col);
	assertTrue("Size:" + col.size(), col.size() >= 2);

	assertEquals(col.toString(), contains, col.contains(new CqlTable("cqldemo", "mybooks")));
	assertEquals(col.toString(), contains, col.contains(new CqlTable("cqldemo", "compoundtest")));
    }

    public void verifyFullAndMinCompletionNotTheSame(
	    ContextCqlCompletion completion,
	    int minMinSize,
	    int fullMinSize) {

	assertNotNull(completion);
	Collection<? extends CqlPart> fullCompletion = asHahsCol(completion.cqlCompletion.fullCompletion);
	assertNotNull(fullCompletion);

	Collection<? extends CqlPart> minCompletion = asHahsCol(completion.cqlCompletion.minCompletion);
	assertNotNull(minCompletion);

	assertTrue(
		"minCompletion:" + minCompletion.size() + ">" + minMinSize,
		minCompletion.size() >= minMinSize);

	assertTrue(
		"fullCompletion:" + fullCompletion.size() + ">" + fullMinSize,
		fullCompletion.size() >= fullMinSize);

	assertFalse(minCompletion.containsAll(fullCompletion));
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

    public void verifyIsEmpty(Collection<?> col) {
	assertNotNull(col);
	assertEquals(0, col.size());
    }

    public void verifyIsEmpty(ContextCqlCompletion completion) {
	assertNotNull(completion);
	assertNotNull(completion.cqlCompletion);
	assertNotNull(completion.cqlCompletion);
	verifyIsEmpty(completion.cqlCompletion.fullCompletion);
	verifyIsEmpty(completion.cqlCompletion.minCompletion);
    }
}

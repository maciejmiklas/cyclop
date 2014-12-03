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
package org.cyclop.service.completion;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.util.Collection;

import javax.inject.Inject;

import org.cyclop.model.ContextCqlCompletion;
import org.cyclop.model.CqlKeyword;
import org.cyclop.model.CqlKeywordValue;
import org.cyclop.model.CqlPart;
import org.cyclop.model.CqlQuery;
import org.cyclop.model.CqlQueryType;
import org.cyclop.model.exception.BeanValidationException;
import org.cyclop.service.cassandra.QueryService;
import org.cyclop.test.AbstractTestCase;
import org.cyclop.test.ValidationHelper;
import org.junit.Ignore;
import org.junit.Test;

import com.google.common.collect.ImmutableSortedSet;

/** @author Maciej Miklas */
public class TestCompletionService extends AbstractTestCase {

	@Inject
	private QueryService qs;

	@Inject
	private ValidationHelper vh;

	@Inject
	private CompletionService cs;

	@Test(expected = BeanValidationException.class)
	public void testFindCompletion_Position_Validation_EmptyQuery() {
		cs.findCompletion(new CqlQuery(CqlQueryType.CREATE_TABLE, " "), 1);
	}

	@Test(expected = BeanValidationException.class)
	public void testFindCompletion_Position_Validation_NullType() {
		cs.findCompletion(new CqlQuery(null, "efqerfqef"), 1);
	}

	@Test(expected = BeanValidationException.class)
	public void testFindCompletion_Position_Validation_NullQuery() {
		cs.findCompletion(new CqlQuery(CqlQueryType.CREATE_TABLE, null), 1);
	}

	@Test(expected = BeanValidationException.class)
	public void testFindCompletion_Position_Validation_NullAll() {
		cs.findCompletion(null, -1);
	}

	@Test(expected = BeanValidationException.class)
	public void testFindCompletion_Validation_EmptyQuery() {
		cs.findCompletion(new CqlQuery(CqlQueryType.CREATE_TABLE, " "));
	}

	@Test(expected = BeanValidationException.class)
	public void testFindCompletion_Validation_NullType() {
		cs.findCompletion(new CqlQuery(null, "efqerfqef"));
	}

	@Test(expected = BeanValidationException.class)
	public void testFindCompletion_Validation_NullQuery() {
		cs.findCompletion(new CqlQuery(CqlQueryType.CREATE_TABLE, null));
	}

	@Test(expected = BeanValidationException.class)
	public void testFindCompletion_Validation_NullAll() {
		cs.findCompletion(null);
	}

	@Test
	public void testFindCompletion_CreateTable_Start() {
		ContextCqlCompletion completion = cs.findCompletion(new CqlQuery(CqlQueryType.CREATE_TABLE, "us"));
		verifyInitialCompletion(completion);
	}

	@Test
	public void testFindCompletion_CreateTable_NotSupported() {
		ContextCqlCompletion completion = cs.findCompletion(new CqlQuery(CqlQueryType.CREATE_TABLE, "create table"));
		vh.verifyCompletionNotSupported(completion, "create table");
	}

	@Test
	public void testFindCompletion_CreateIndex_Start() {
		ContextCqlCompletion completion = cs.findCompletion(new CqlQuery(CqlQueryType.CREATE_INDEX, "us"));
		verifyInitialCompletion(completion);
	}

	@Test
	public void testFindCompletion_CreateIndex_NotSupported() {
		ContextCqlCompletion completion = cs.findCompletion(new CqlQuery(CqlQueryType.BATCH, "create index"));
		vh.verifyCompletionNotSupported(completion, "create index");
	}

	@Test
	public void testFindCompletion_Batch_Start() {
		ContextCqlCompletion completion = cs.findCompletion(new CqlQuery(CqlQueryType.BATCH, "us"));
		verifyInitialCompletion(completion);
	}

	@Test
	public void testFindCompletion_Batch_NotSupported() {
		ContextCqlCompletion completion = cs.findCompletion(new CqlQuery(CqlQueryType.BATCH, "batch"));
		vh.verifyCompletionNotSupported(completion, "batch");
	}

	@Test
	public void testFindCompletion_AlterTable_Start() {
		ContextCqlCompletion completion = cs.findCompletion(new CqlQuery(CqlQueryType.ALTER_TABLE, "us"));
		verifyInitialCompletion(completion);
	}

	@Test
	public void testFindCompletion_AlterTable_NotSupported() {
		ContextCqlCompletion completion = cs.findCompletion(new CqlQuery(CqlQueryType.ALTER_KEYSPACE, "alter Table"));
		vh.verifyCompletionNotSupported(completion, "alter table");
	}

	@Test
	public void testFindCompletion_AlterKeyspace_Start() {
		ContextCqlCompletion completion = cs.findCompletion(new CqlQuery(CqlQueryType.ALTER_KEYSPACE, "us"));
		verifyInitialCompletion(completion);
	}

	@Test
	public void testFindCompletion_AlterKeyspace_NotSupported() {
		ContextCqlCompletion completion = cs
				.findCompletion(new CqlQuery(CqlQueryType.ALTER_KEYSPACE, "alter keyspace"));
		vh.verifyCompletionNotSupported(completion, "alter keyspace");
	}

	@Test
	public void testFindCompletion_Use_Start() {
		ContextCqlCompletion completion = cs.findCompletion(new CqlQuery(CqlQueryType.USE, "us"));
		verifyInitialCompletion(completion);
	}

	@Test
	public void testFindCompletion_Use_AfterUse() {
		ContextCqlCompletion completion = cs.findCompletion(new CqlQuery(CqlQueryType.USE, "use "));
		vh.verifyFullAndMinCompletionTheSame(completion, 3);
		vh.verifyContainsAllKeyspaces(completion.cqlCompletion.minCompletion, true);
	}

	@Test
	public void testFindCompletion_Update_Start() {
		ContextCqlCompletion completion = cs.findCompletion(new CqlQuery(CqlQueryType.UPDATE, "up..."));
		verifyInitialCompletion(completion);
	}

	@Test
	public void testFindCompletion_Update_AfterUpdate() {
		qs.execute(new CqlQuery(CqlQueryType.USE, "use system"));
		ContextCqlCompletion completion = cs.findCompletion(new CqlQuery(CqlQueryType.UPDATE, "update"));
		vh.verifyContainsAllKeyspacesAndTables(completion, false);
	}

	@Test
	public void testFindCompletion_Update_AfterTableName() {
		qs.execute(new CqlQuery(CqlQueryType.USE, "use cqldemo"));
		ContextCqlCompletion completion = cs.findCompletion(new CqlQuery(CqlQueryType.UPDATE, "update mybooks"));
		vh.verifyFullAndMinCompletionTheSame(completion, 3);
		vh.verifyContainsOnlyKeywords(completion.cqlCompletion.minCompletion, CqlKeyword.Def.USING_TTL.value,
				CqlKeyword.Def.USING_TIMESTAMP.value, CqlKeyword.Def.SET.value);
	}

	@Test
	public void testFindCompletion_Update_AfterSet() {
		qs.execute(new CqlQuery(CqlQueryType.USE, "use cqldemo"));
		ContextCqlCompletion completion = cs.findCompletion(new CqlQuery(CqlQueryType.UPDATE,
				"update mybooks USING TTL 400 SET director = 'Joss Whedon',"));

		vh.verifyFullAndMinCompletionTheSame(completion, 14);

		Collection<? extends CqlPart> cmp = vh.asHahsCol(completion.cqlCompletion.fullCompletion);
		vh.verifyContainsAllKeyspaces(cmp, false);
		vh.verifyContainsOnlyKeywords(cmp, CqlKeyword.Def.WHERE.value);
		vh.verifyContainsTableNamesCqlDemo(cmp, false);
		vh.verifyContainsTableNamesSystem(cmp, false);
		vh.verifyContainsIndexFromCqlDemo(cmp, false);

		vh.verifyContainsMybooksColumns(cmp, true);
		vh.verifyContainsSystemColumns(cmp, false);
	}

	@Test
	public void testFindCompletion_Update_AfterWhere() {
		qs.execute(new CqlQuery(CqlQueryType.USE, "use cqldemo"));
		ContextCqlCompletion completion = cs
				.findCompletion(new CqlQuery(CqlQueryType.UPDATE,
						"update mybooks USING TTL 400 SET director = 'Joss Whedon',main_actor = 'Nathan Fillion',year = 2005 where"));

		vh.verifyFullAndMinCompletionTheSame(completion, 14);

		Collection<? extends CqlPart> cmp = vh.asHahsCol(completion.cqlCompletion.fullCompletion);
		vh.verifyContainsAllKeyspaces(cmp, false);
		vh.verifyContainsOnlyKeywords(cmp, CqlKeyword.Def.AND.value);
		vh.verifyContainsTableNamesCqlDemo(cmp, false);
		vh.verifyContainsTableNamesSystem(cmp, false);
		vh.verifyContainsIndexFromCqlDemo(cmp, false);
		vh.verifyContainsMybooksColumns(cmp, true);
		vh.verifyContainsSystemColumns(cmp, false);
	}

	@Test
	public void testFindCompletion_Truncate_Start() {
		ContextCqlCompletion completion = cs.findCompletion(new CqlQuery(CqlQueryType.TRUNCATE, "bara-bara"));
		verifyInitialCompletion(completion);
	}

	@Test
	public void testFindCompletion_Truncate_AfterTruncate() {
		qs.execute(new CqlQuery(CqlQueryType.USE, "use system"));

		ContextCqlCompletion completion = cs.findCompletion(new CqlQuery(CqlQueryType.TRUNCATE, "truncate "));
		vh.verifyContainsAllKeyspacesAndTables(completion, false);
	}

	@Test
	public void testFindCompletion_Truncate_AfterTruncateWithSpaceName() {
		qs.execute(new CqlQuery(CqlQueryType.USE, "use system"));

		ContextCqlCompletion completion = cs.findCompletion(new CqlQuery(CqlQueryType.TRUNCATE, "truncate cqldemo. "));
		vh.verifyFullAndMinCompletionNotTheSame(completion, 2, 4);
		vh.verifyCqldemoSpaceInQuery(completion);
	}

	@Test
	public void testFindCompletion_Insert_Start() {
		ContextCqlCompletion completion = cs.findCompletion(new CqlQuery(CqlQueryType.INSERT, "inse..."));
		verifyInitialCompletion(completion);
	}

	@Test
	public void testFindCompletion_Insert_AfterInsert_SpaceSystem() {
		qs.execute(new CqlQuery(CqlQueryType.USE, "use system"));
		ContextCqlCompletion completion = cs.findCompletion(new CqlQuery(CqlQueryType.INSERT, "insert into abc..."));

		vh.verifyContainsAllKeyspacesAndTables(completion, false);
	}

	@Test
	public void testFindCompletion_Insert_AfterTableName() {
		qs.execute(new CqlQuery(CqlQueryType.USE, "use cqldemo"));
		ContextCqlCompletion completion = cs.findCompletion(new CqlQuery(CqlQueryType.INSERT,
				"insert into cqldemo.mybooks ("));

		vh.verifyFullAndMinCompletionTheSame(completion, 13);

		{
			Collection<? extends CqlPart> mcmp = vh.asHahsCol(completion.cqlCompletion.minCompletion);
			vh.verifyContainsAllKeyspaces(mcmp, false);
			vh.verifyContainsNoKeywords(mcmp);
			vh.verifyContainsTableNamesCqlDemo(mcmp, false);
			vh.verifyContainsTableNamesSystem(mcmp, false);
			vh.verifyContainsIndexFromCqlDemo(mcmp, false);
			vh.verifyContainsMybooksColumns(mcmp, true);
			vh.verifyContainsSystemColumns(mcmp, false);
		}

		{
			Collection<? extends CqlPart> fcmp = vh.asHahsCol(completion.cqlCompletion.fullCompletion);
			vh.verifyContainsAllKeyspaces(fcmp, false);
			vh.verifyContainsNoKeywords(fcmp);
			vh.verifyContainsTableNamesCqlDemo(fcmp, false);
			vh.verifyContainsTableNamesSystem(fcmp, false);
			vh.verifyContainsIndexFromCqlDemo(fcmp, false);
			vh.verifyContainsMybooksColumns(fcmp, true);
			vh.verifyContainsSystemColumns(fcmp, false);
		}
	}

	@Test
	public void testFindCompletion_Insert_AfterColumns() {
		qs.execute(new CqlQuery(CqlQueryType.USE, "use cqldemo"));
		ContextCqlCompletion completion = cs.findCompletion(new CqlQuery(CqlQueryType.INSERT,
				"insert into cqldemo.mybooks (a,b,c)"));

		vh.verifyFullAndMinCompletionTheSame(completion, 1);
		vh.verifyContainsOnlyKeywords(completion.cqlCompletion.fullCompletion, CqlKeyword.Def.VALUES.value);
	}

	@Test
	public void testFindCompletion_Insert_OnValues() {
		qs.execute(new CqlQuery(CqlQueryType.USE, "use cqldemo"));
		ContextCqlCompletion completion = cs.findCompletion(new CqlQuery(CqlQueryType.INSERT,
				"insert into cqldemo.mybooks (a,b,c) values"));

		vh.verifyFullAndMinCompletionTheSame(completion, 1);
		vh.verifyContainsOnlyKeywords(completion.cqlCompletion.fullCompletion, CqlKeyword.Def.VALUES.value);
	}

	@Test
	public void testFindCompletion_Insert_AfterVaues() {
		qs.execute(new CqlQuery(CqlQueryType.USE, "use cqldemo"));
		ContextCqlCompletion completion = cs.findCompletion(new CqlQuery(CqlQueryType.INSERT,
				"insert into cqldemo.mybooks (a,b,c) values ('1','1','bla','2') "));

		vh.verifyFullAndMinCompletionTheSame(completion, 3);
		vh.verifyContainsOnlyKeywords(completion.cqlCompletion.fullCompletion, CqlKeyword.Def.USING_TTL.value,
				CqlKeyword.Def.USING_TIMESTAMP.value, CqlKeyword.Def.AND.value);
	}

	@Test
	public void testFindCompletion_Insert_AfterInsert_SpaceCqlDemo() {
		qs.execute(new CqlQuery(CqlQueryType.USE, "use cqldemo"));
		ContextCqlCompletion completion = cs.findCompletion(new CqlQuery(CqlQueryType.INSERT, "insert into abc..."));

		vh.verifyContainsAllKeyspacesAndTables(completion, true);
	}

	@Test
	public void testFindCompletion_CreateKeyspace_AfterCreate() {
		ContextCqlCompletion completion = cs.findCompletion(new CqlQuery(CqlQueryType.CREATE_KEYSPACE,
				"create keyspace "));

		vh.verifyFullAndMinCompletionTheSame(completion, 2);

		ImmutableSortedSet<? extends CqlPart> cmp = completion.cqlCompletion.fullCompletion;
		vh.verifyContainsOnlyKeywords(cmp, CqlKeyword.Def.WITH.value, CqlKeyword.Def.IF_NOT_EXISTS.value);
	}

	@Test
	public void testFindCompletion_CreateKeyspace_AfterWith() {
		verifyCreateKeyspaceWith("create keyspace with ");
	}

	@Test
	public void testFindCompletion_CreateKeyspace_ExistsWith() {
		verifyCreateKeyspaceWith("create keyspace inf not exists with ");
	}

	@Test
	public void testFindCompletion_CreateKeyspace_Start() {
		ContextCqlCompletion completion = cs.findCompletion(new CqlQuery(CqlQueryType.CREATE_KEYSPACE, "c"));
		verifyInitialCompletion(completion);
	}

	@Test
	public void testFindCompletion_CreateKeyspace_StartAfterCreate() {
		ContextCqlCompletion completion = cs.findCompletion(new CqlQuery(CqlQueryType.CREATE_KEYSPACE, "CREATE "));
		verifyInitialCompletion(completion);
	}

	@Test
	public void testFindCompletion_Delete_AfterDelete() {
		ContextCqlCompletion completion = cs.findCompletion(new CqlQuery(CqlQueryType.DELETE, "delete "));

		vh.verifyFullAndMinCompletionTheSame(completion, 30);
		vh.verifyContainsAllColumns(completion, true);

		ImmutableSortedSet<? extends CqlPart> cmp = completion.cqlCompletion.fullCompletion;
		vh.verifyContainsOnlyKeywords(cmp, CqlKeyword.Def.FROM.value);
	}

	@Test
	public void testFindCompletion_Delete_AfterFrom_NoSpqceInQuery() {
		qs.execute(new CqlQuery(CqlQueryType.USE, "use cqldemo"));

		ContextCqlCompletion completion = cs.findCompletion(new CqlQuery(CqlQueryType.DELETE, "delete abc from "));

		verifyAfterFromNoSpqceInQuery(completion);
	}

	@Test
	public void testFindCompletion_Delete_AfterFrom_SpaceInQuery() {
		qs.execute(new CqlQuery(CqlQueryType.USE, "use system"));

		ContextCqlCompletion completion = cs.findCompletion(new CqlQuery(CqlQueryType.DELETE,
				"delete abc from cqldemo."));

		vh.verifyCqldemoSpaceInQuery(completion);
	}

	@Test
	public void testFindCompletion_Delete_AfterTableName_KeyspaceInQuery() {
		ContextCqlCompletion completion = cs.findCompletion(new CqlQuery(CqlQueryType.DELETE,
				"delete abc from cqldemo.mybooks"));
		verifyDeleteAfterTableName(completion);
	}

	@Test
	public void testFindCompletion_Delete_AfterTableName_KeyspaceInSession() {
		qs.execute(new CqlQuery(CqlQueryType.USE, "use cqldemo"));
		ContextCqlCompletion completion = cs
				.findCompletion(new CqlQuery(CqlQueryType.DELETE, "delete abc from mybooks"));
		verifyDeleteAfterTableName(completion);
	}

	@Test
	public void testFindCompletion_Delete_AfterTableName_WrongKeyspaceInSession() {
		qs.execute(new CqlQuery(CqlQueryType.USE, "use system"));
		ContextCqlCompletion completion = cs
				.findCompletion(new CqlQuery(CqlQueryType.DELETE, "delete abc from mybOoks"));
		verifyDeleteAfterTableName(completion);
	}

	@Test
	public void testFindCompletion_Delete_AfterWhere() {
		ContextCqlCompletion completion = cs.findCompletion(new CqlQuery(CqlQueryType.DELETE,
				"delete abc from cqldemo.mybooks WHERE"));

		vh.verifyFullAndMinCompletionNotTheSame(completion, 15, 15);

		{
			Collection<? extends CqlPart> mcmp = vh.asHahsCol(completion.cqlCompletion.minCompletion);
			vh.verifyContainsOnlyKeywords(mcmp, CqlKeyword.Def.IN.value, CqlKeyword.Def.AND.value);
		}

		{
			Collection<? extends CqlPart> fcmp = vh.asHahsCol(completion.cqlCompletion.fullCompletion);
			vh.verifyContainsMybooksColumns(fcmp, true);
			vh.verifyContainsAllKeyspaces(fcmp, false);
			vh.verifyContainsTableNamesSystem(fcmp, false);
			vh.verifyContainsCompoundTestColumns(fcmp, false);
			vh.verifyContainsOnlyKeywords(fcmp, CqlKeyword.Def.IN_BL.value, CqlKeyword.Def.AND.value);
		}
	}

	@Test
	public void testFindCompletion_Delete_Start() {
		ContextCqlCompletion completion = cs.findCompletion(new CqlQuery(CqlQueryType.DELETE, "d"));
		verifyInitialCompletion(completion);
	}

	@Test
	public void testFindCompletion_DropIndex_AfterDrop() {
		qs.execute(new CqlQuery(CqlQueryType.USE, "use cqldemo"));
		ContextCqlCompletion completion = cs.findCompletion(new CqlQuery(CqlQueryType.DROP_INDEX, "drop index   "));
		verifyDropIndexAfterDrop(completion);
	}

	@Test
	public void testFindCompletion_DropIndex_AfterDropWithExists() {
		qs.execute(new CqlQuery(CqlQueryType.USE, "use cqldemo"));
		ContextCqlCompletion completion = cs.findCompletion(new CqlQuery(CqlQueryType.DROP_INDEX,
				"drop index IF NOT EXISTS"));
		verifyDropIndexAfterDrop(completion);
	}

	@Test
	public void testFindCompletion_DropIndex_Start() {
		ContextCqlCompletion completion = cs.findCompletion(new CqlQuery(CqlQueryType.DROP_INDEX, "d"));
		verifyInitialCompletion(completion);
	}

	@Test
	public void testFindCompletion_DropIndex_StartDrop() {
		ContextCqlCompletion completion = cs.findCompletion(new CqlQuery(CqlQueryType.DROP_INDEX, "drop"));
		verifyInitialCompletion(completion);
	}

	@Test
	public void testFindCompletion_DropKeyspace_AfterDrop() {
		ContextCqlCompletion completion = cs.findCompletion(new CqlQuery(CqlQueryType.DROP_KEYSPACE, "drop keyspace"));
		verifyDropKeyspaceAfterDrop(completion);
	}

	@Test
	public void testFindCompletion_DropKeyspace_AfterDrop_KeyspacePartialyInQuery() {
		ContextCqlCompletion completion = cs.findCompletion(new CqlQuery(CqlQueryType.DROP_KEYSPACE,
				"drop keyspace cqlde"));
		verifyDropKeyspaceAfterDrop(completion);
	}

	@Test
	public void testFindCompletion_DropKeyspace_Start() {
		ContextCqlCompletion completion = cs.findCompletion(new CqlQuery(CqlQueryType.DROP_KEYSPACE, "d"));
		verifyInitialCompletion(completion);
	}

	@Test
	public void testFindCompletion_DropKeyspace_StartDrop() {
		ContextCqlCompletion completion = cs.findCompletion(new CqlQuery(CqlQueryType.DROP_KEYSPACE, "drop"));
		verifyInitialCompletion(completion);
	}

	@Test
	public void testFindCompletion_DropTable_AfterDrop_SpaceCqlDemo() {
		qs.execute(new CqlQuery(CqlQueryType.USE, "use cqldemo"));
		ContextCqlCompletion completion = cs.findCompletion(new CqlQuery(CqlQueryType.DROP_TABLE, "drop table "));
		verifyDropTableAfterDrop(completion, true);
	}

	@Test
	public void testFindCompletion_DropTable_AfterDrop_SpaceCqlDemo_IfExists() {
		qs.execute(new CqlQuery(CqlQueryType.USE, "use cqldemo"));
		ContextCqlCompletion completion = cs.findCompletion(new CqlQuery(CqlQueryType.DROP_TABLE,
				"drop table if exists a"));
		verifyDropTableAfterDrop(completion, true);
	}

	@Test
	public void testFindCompletion_DropTable_AfterDrop_SpaceSystem() {
		qs.execute(new CqlQuery(CqlQueryType.USE, "use system"));
		ContextCqlCompletion completion = cs.findCompletion(new CqlQuery(CqlQueryType.DROP_TABLE, "drop table "));
		verifyDropTableAfterDrop(completion, false);
	}

	@Test
	public void testFindCompletion_DropTable_AfterDrop_SpaceSystem_IfExists() {
		qs.execute(new CqlQuery(CqlQueryType.USE, "use system"));
		ContextCqlCompletion completion = cs.findCompletion(new CqlQuery(CqlQueryType.DROP_TABLE,
				"drop table if exists"));
		verifyDropTableAfterDrop(completion, false);
	}

	private void verifyDropTableAfterDrop(ContextCqlCompletion completion, boolean spaceCqlDemoOrSystem) {
		vh.verifyFullAndMinCompletionNotTheSame(completion, 6, 6);
		vh.verifyContainsAllColumns(completion, false);

		{
			Collection<? extends CqlPart> mcmp = vh.asHahsCol(completion.cqlCompletion.minCompletion);
			vh.verifyContainsAllKeyspaces(mcmp, true);
			vh.verifyContainsOnlyKeywords(mcmp, CqlKeyword.Def.IF_EXISTS.value);
			vh.verifyContainsTableNamesCqlDemo(mcmp, spaceCqlDemoOrSystem);
			vh.verifyContainsTableNamesSystem(mcmp, !spaceCqlDemoOrSystem);
			vh.verifyContainsIndexFromCqlDemo(mcmp, false);
		}

		{
			Collection<? extends CqlPart> fcmp = vh.asHahsCol(completion.cqlCompletion.fullCompletion);
			vh.verifyContainsAllKeyspaces(fcmp, true, ".");
			vh.verifyContainsOnlyKeywords(fcmp, CqlKeyword.Def.IF_EXISTS.value);
			vh.verifyContainsTableNamesCqlDemo(fcmp, spaceCqlDemoOrSystem);
			vh.verifyContainsTableNamesSystem(fcmp, !spaceCqlDemoOrSystem);
			vh.verifyContainsIndexFromCqlDemo(fcmp, false);
		}
	}

	@Test
	public void testFindCompletion_DropTable_Start() {
		ContextCqlCompletion completion = cs.findCompletion(new CqlQuery(CqlQueryType.DROP_TABLE, "dro"));
		verifyInitialCompletion(completion);
	}

	@Test
	public void testFindCompletion_Select_AfterFrom_ContainsColumnsFromKeyspaceInSessionScope() {
		qs.execute(new CqlQuery(CqlQueryType.USE, "use cqldemo"));
		ContextCqlCompletion completion = cs.findCompletion(new CqlQuery(CqlQueryType.SELECT, "select * from mybooks"),
				8);
		verifySelectAfterFrom(completion);
	}

	@Test
	public void testFindCompletion_Select_AfterFrom_ContainsColumnsFromKeyspaceWithTable_CursorOnEnd() {
		ContextCqlCompletion completion = cs.findCompletion(new CqlQuery(CqlQueryType.SELECT,
				"select * from cqldemo.mybooks"), 8);
		verifySelectAfterFrom(completion);
	}

	@Test
	public void testFindCompletion_Select_AfterFrom_ContainsColumnsFromKeyspaceWithTable_CursorOnFrom() {
		ContextCqlCompletion completion = cs.findCompletion(new CqlQuery(CqlQueryType.SELECT,
				"select * from cqldemo.mybooks"), 10);
		verifySelectAfterFrom(completion);
	}

	@Test
	public void testFindCompletion_Select_AfterFrom_NoSpqceInQuery() {
		qs.execute(new CqlQuery(CqlQueryType.USE, "use cqldemo"));
		ContextCqlCompletion completion = cs.findCompletion(new CqlQuery(CqlQueryType.SELECT, "select * from "));
		verifyAfterFromNoSpqceInQuery(completion);
	}

	@Test
	public void testFindCompletion_Select_AfterFrom_SelectTableFromDifferentSpace() {
		qs.execute(new CqlQuery(CqlQueryType.USE, "use system"));
		ContextCqlCompletion completion = cs.findCompletion(new CqlQuery(CqlQueryType.SELECT, "select * from mybooks"),
				8);
		verifySelectAfterFrom(completion);
	}

	@Test
	public void testFindCompletion_Select_AfterFrom_SpaceInQuery() {
		qs.execute(new CqlQuery(CqlQueryType.USE, "use system"));
		ContextCqlCompletion completion = cs
				.findCompletion(new CqlQuery(CqlQueryType.SELECT, "select * from cqldemo."));
		vh.verifyCqldemoSpaceInQuery(completion);
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
		ContextCqlCompletion completion = cs.findCompletion(new CqlQuery(CqlQueryType.SELECT, "select *"), 88);

		vh.verifyFullAndMinCompletionTheSame(completion, 30);
		vh.verifyContainsAllColumns(completion, true);
	}

	@Test
	public void testFindCompletion_Select_AfterTableName() {
		ContextCqlCompletion completion = cs.findCompletion(new CqlQuery(CqlQueryType.SELECT,
				"select * from cqldemo.mybooks "), 30);
		vh.verifyFullAndMinCompletionTheSame(completion, 4);

		ImmutableSortedSet<? extends CqlPart> cmp = completion.cqlCompletion.fullCompletion;
		vh.verifyContainsAllColumns(completion, false);

		vh.verifyContainsOnlyKeywords(cmp, CqlKeyword.Def.WHERE.value, CqlKeyword.Def.LIMIT.value,
				CqlKeyword.Def.ORDER_BY.value, CqlKeyword.Def.ALLOW_FILTERING.value);
	}

	@Test
	public void testFindCompletion_Select_AfterWhere() {
		ContextCqlCompletion completion = cs.findCompletion(new CqlQuery(CqlQueryType.SELECT,
				"select * from cqldemo.mybooks  where "), 37);
		vh.verifyFullAndMinCompletionNotTheSame(completion, 18, 18);

		{
			ImmutableSortedSet<? extends CqlPart> fcmp = completion.cqlCompletion.fullCompletion;
			vh.verifyContainsMybooksColumns(fcmp, true);
			vh.verifyContainsAllKeyspaces(fcmp, false);
			vh.verifyContainsTableNamesSystem(fcmp, false);
			vh.verifyContainsCompoundTestColumns(fcmp, false);

			vh.verifyContainsOnlyKeywords(fcmp, CqlKeyword.Def.ORDER_BY.value, CqlKeyword.Def.LIMIT.value,
					CqlKeyword.Def.IN_BL.value, CqlKeyword.Def.AND.value, CqlKeyword.Def.ALLOW_FILTERING.value);
		}

		{
			ImmutableSortedSet<? extends CqlPart> mcmp = completion.cqlCompletion.minCompletion;
			vh.verifyContainsOnlyKeywords(mcmp, CqlKeyword.Def.ORDER_BY.value, CqlKeyword.Def.LIMIT.value,
					CqlKeyword.Def.IN.value, CqlKeyword.Def.AND.value, CqlKeyword.Def.ALLOW_FILTERING.value);
		}
	}

	@Test
	public void testFindInitialCompletion() {
		ContextCqlCompletion completion = cs.findInitialCompletion();
		verifyInitialCompletion(completion);
	}

	@Test
	public void testFindCompletion_CursorOn0() {
		ContextCqlCompletion completion = cs.findCompletion(new CqlQuery(CqlQueryType.SELECT,
				"select * from cqldemo.mybooks  where "), 0);
		verifyInitialCompletion(completion);
	}

	private void veifySelectWithOrderBy(String cql) {
		ContextCqlCompletion completion = cs.findCompletion(new CqlQuery(CqlQueryType.SELECT, cql), -1);
		vh.verifyFullAndMinCompletionTheSame(completion, 18);

		ImmutableSortedSet<? extends CqlPart> cmp = completion.cqlCompletion.fullCompletion;
		vh.verifyContainsMybooksColumns(cmp, true);
		vh.verifyContainsAllKeyspaces(cmp, false);
		vh.verifyContainsTableNamesSystem(cmp, false);
		vh.verifyContainsCompoundTestColumns(cmp, false);

		vh.verifyContainsOnlyKeywords(cmp, CqlKeyword.Def.TOKEN.value, CqlKeyword.Def.LIMIT.value,
				CqlKeyword.Def.DESC.value, CqlKeyword.Def.ASC.value, CqlKeyword.Def.ALLOW_FILTERING.value);
	}

	private void verifyAfterFromNoSpqceInQuery(ContextCqlCompletion completion) {
		vh.verifyFullAndMinCompletionNotTheSame(completion, 2, 4);

		{
			ImmutableSortedSet<? extends CqlPart> mcmp = completion.cqlCompletion.minCompletion;
			vh.verifyContainsTableNamesCqlDemo(mcmp, true);
			vh.verifyContainsTableNamesSystem(mcmp, false);
			vh.verifyContainsTableNamesWithSpaceCqlDemo(mcmp, false);
			vh.verifyContainsAllKeyspaces(mcmp, true);
		}

		{
			ImmutableSortedSet<? extends CqlPart> fullCompletion = completion.cqlCompletion.fullCompletion;
			vh.verifyContainsTableNamesCqlDemo(fullCompletion, true);
			vh.verifyContainsTableNamesSystem(fullCompletion, false);
			vh.verifyContainsTableNamesWithSpaceCqlDemo(fullCompletion, false);
			vh.verifyContainsAllKeyspaces(fullCompletion, true, ".");
		}
	}

	private void verifyContainsFromKeywords(Collection<? extends CqlPart> col, boolean contains) {
		col = vh.asHahsCol(col);
		assertNotNull(col);
		assertEquals(col.toString(), contains, col.contains(CqlKeyword.Def.FROM.value));
		assertEquals(col.toString(), contains, col.contains(CqlKeyword.Def.COUNT_AST.value));
		assertEquals(col.toString(), contains, col.contains(CqlKeyword.Def.COUNT_ONE.value));
	}

	private void verifyCreateKeyspaceWith(String cql) {
		ContextCqlCompletion completion = cs.findCompletion(new CqlQuery(CqlQueryType.CREATE_KEYSPACE, cql));

		vh.verifyFullAndMinCompletionNotTheSame(completion, 10, 34);

		{
			ImmutableSortedSet<? extends CqlPart> mcmp = completion.cqlCompletion.minCompletion;
			vh.verifyContainsOnlyKeywords(mcmp, CqlKeywordValue.Def.TRUE.value,
					CqlKeywordValue.Def.SIMPLE_STRATEGY.value, CqlKeywordValue.Def.REPLICATION_FACTOR.value,
					CqlKeyword.Def.REPLICATION.value, CqlKeywordValue.Def.OLD_NETWORK_TOPOLOGY_STRATEGY.value,
					CqlKeywordValue.Def.NETWORK_TOPOLOGY_STRATEGY.value, CqlKeywordValue.Def.FALSE.value,
					CqlKeywordValue.Def.DURABLE_WRITES.value, CqlKeywordValue.Def.CLASS.value, CqlKeyword.Def.AND.value);
		}

		{
			ImmutableSortedSet<? extends CqlPart> fcmp = completion.cqlCompletion.fullCompletion;
			assertTrue(fcmp.toString(), fcmp.contains(new CqlPart(",oldnetworktopologystrategy")));
			assertTrue(fcmp.toString(), fcmp.contains(new CqlPart("(class")));
			assertTrue(fcmp.toString(), fcmp.contains(new CqlPart(":replication_factor")));
			assertTrue(fcmp.toString(), fcmp.contains(new CqlPart(":class")));
		}
	}

	private void verifyDeleteAfterTableName(ContextCqlCompletion completion) {
		vh.verifyContainsAllColumns(completion, false);
		vh.verifyFullAndMinCompletionTheSame(completion, 2);

		ImmutableSortedSet<? extends CqlPart> cmp = completion.cqlCompletion.fullCompletion;
		vh.verifyContainsMybooksColumns(cmp, false);

		vh.verifyContainsOnlyKeywords(cmp, CqlKeyword.Def.WHERE.value, CqlKeyword.Def.USING_TIMESTAMP.value);
	}

	private void verifyDropIndexAfterDrop(ContextCqlCompletion completion) {
		vh.verifyFullAndMinCompletionTheSame(completion, 5);

		ImmutableSortedSet<? extends CqlPart> cmp = completion.cqlCompletion.fullCompletion;
		vh.verifyContainsIndexFromCqlDemo(cmp, true);
		vh.verifyContainsOnlyKeywords(cmp, CqlKeyword.Def.IF_NOT_EXISTS.value);
	}

	private void verifyDropKeyspaceAfterDrop(ContextCqlCompletion completion) {
		vh.verifyFullAndMinCompletionTheSame(completion, 4);
		Collection<? extends CqlPart> cmp = vh.asHahsCol(completion.cqlCompletion.fullCompletion);
		vh.verifyContainsAllKeyspaces(cmp, true);
		vh.verifyContainsAllColumns(completion, false);
		vh.verifyContainsOnlyKeywords(cmp, CqlKeyword.Def.IF_EXISTS.value);
	}

	private void verifyInitialCompletion(ContextCqlCompletion completion) {
		vh.verifyFullAndMinCompletionTheSame(completion, 15);

		ImmutableSortedSet<? extends CqlPart> cmp = completion.cqlCompletion.fullCompletion;
		vh.verifyContainsOnlyKeywords(cmp, CqlKeyword.Def.USE.value, CqlKeyword.Def.UPDATE.value,
				CqlKeyword.Def.TRUNCATE.value, CqlKeyword.Def.SELECT.value, CqlKeyword.Def.INSERT_INTO.value,
				CqlKeyword.Def.DROP_TABLE.value, CqlKeyword.Def.DROP_KEYSPACE.value, CqlKeyword.Def.DROP_INDEX.value,
				CqlKeyword.Def.DELETE.value, CqlKeyword.Def.CREATE_KEYSPACE.value);
	}

	private void verifySelectAfterFrom(ContextCqlCompletion completion) {
		vh.verifyFullAndMinCompletionTheSame(completion, 10);
		ImmutableSortedSet<? extends CqlPart> cmp = completion.cqlCompletion.fullCompletion;

		vh.verifyContainsMybooksColumns(cmp, true);
		verifyContainsFromKeywords(cmp, true);
		vh.verifyContainsSystemColumns(cmp, false);
		vh.verifyContainsCompoundTestColumns(cmp, false);
	}

}

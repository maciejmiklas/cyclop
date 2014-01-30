package org.cyclop.service.cassandra;

import com.datastax.driver.core.DataType;
import com.datastax.driver.core.Row;
import com.google.common.collect.ImmutableSortedSet;
import org.cyclop.model.*;
import org.cyclop.test.AbstractTestCase;
import org.cyclop.test.ValidationHelper;
import org.junit.Test;

import javax.inject.Inject;
import java.util.UUID;

import static junit.framework.Assert.assertNotNull;
import static junit.framework.Assert.assertTrue;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;

/**
 * @author Maciej Miklas
 */
public class TestCassandraServiceImpl extends AbstractTestCase {

    @Inject
    private QueryService qs;

    @Inject
    private ValidationHelper vh;

    @Test
    public void testFindColumnNames_TableDoesNotExist() {
        ImmutableSortedSet<CqlColumnName> col = qs.findColumnNames(new CqlTable("not-existing"));
        vh.verifyEmpty(col);
    }

    @Test
    public void testFindColumnNames_KeyspaceWithTable() {
        ImmutableSortedSet<CqlColumnName> resp = qs.findColumnNames(new CqlTable("cqldemo", "MyBooks"));
        assertNotNull(resp);
        assertTrue("size: " + resp.size(), resp.size() > 5);
        vh.verifyMybooksColumns(resp, true);
        vh.verifySystemColumns(resp, false);
        vh.verifyCompoundTestColumns(resp, false);
    }

    @Test
    public void testFindTableNames_SpaceCqlDemo() {
        ImmutableSortedSet<CqlTable> col = qs.findTableNames(new CqlKeySpace("cqldemo"));
        vh.verifyTableNamesCqlDemo(col, true);
    }

    @Test
    public void testFindTableNames_SpaceSystem() {
        ImmutableSortedSet<CqlTable> col = qs.findTableNames(new CqlKeySpace("system"));
        vh.verifyTableNamesSystem(col, true);
    }

    @Test
    public void testFindTableNames_SpaceDoesNotExist() {
        ImmutableSortedSet<CqlTable> col = qs.findTableNames(new CqlKeySpace("abcx"));
        vh.verifyEmpty(col);
    }

    @Test
    public void testFindAllIndexes_CqlDemo() {
        ImmutableSortedSet<CqlIndex> index = qs.findAllIndexes(new CqlKeySpace("cqldemo"));
        vh.verifyIndexFromCqlDemo(index, true);
    }

    @Test
    public void testFindAllKeySpaces() {
        ImmutableSortedSet<CqlKeySpace> kss = qs.findAllKeySpaces();
        vh.verifyKeyspaces(kss, true);
    }

    @Test
    public void testFindAllIndexes_KeyspaceDoesNotExist() {
        ImmutableSortedSet<CqlIndex> index = qs.findAllIndexes(new CqlKeySpace("space..."));
        vh.verifyEmpty(index);
    }

    @Test
    public void testFindColumnNames_KeyspaceInSession() {
        qs.execute(new CqlQuery(CqlQueryName.USE, "use cqldemo"));
        ImmutableSortedSet<CqlColumnName> resp = qs.findColumnNames(new CqlTable("MyBooks"));
        assertNotNull(resp);
        assertTrue("size: " + resp.size(), resp.size() > 5);
        vh.verifyMybooksColumns(resp, true);
        vh.verifySystemColumns(resp, false);
        vh.verifyCompoundTestColumns(resp, false);
    }

    @Test
    public void testFindAllColumnNames() {
        ImmutableSortedSet<CqlColumnName> allColumnNames = qs.findAllColumnNames();
        assertNotNull(allColumnNames);
        assertFalse(allColumnNames.isEmpty());

        vh.verifyMybooksColumns(allColumnNames, true);
        vh.verifySystemColumns(allColumnNames, true);
        vh.verifyCompoundTestColumns(allColumnNames, true);
    }

    @Test
    public void testExecuteCompoundPkNoDynamicColumns() {
        // create data
        {
            qs.execute(new CqlQuery(CqlQueryName.USE, "USE CqlDemo"));
            for (int i = 0; i < 50; i++) {
                StringBuilder cql = new StringBuilder("INSERT INTO CompoundTest (id,id2,id3,deesc) VALUES (");
                cql.append(UUID.randomUUID()).append(",");
                cql.append(i);
                cql.append(",'abc','some text-" + i + "')");
                qs.execute(new CqlQuery(CqlQueryName.INSERT, cql.toString()));
            }
        }

        CqlSelectResult res = qs.execute(new CqlQuery(CqlQueryName.SELECT, "select * from CompoundTest"));
        assertEquals(50, res.rows.size());

        assertEquals(4, res.commonColumns.size());
        assertEquals(0, res.dynamicColumns.size());

        String comColsStr = res.commonColumns.toString();
        assertTrue(comColsStr,
                res.commonColumns.contains(new CqlExtendedColumnName(CqlColumnType.PARTITION_KEY, DataType.uuid(), "id")));

        assertTrue(comColsStr,
                res.commonColumns.contains(new CqlExtendedColumnName(CqlColumnType.CLUSTERING_KEY, DataType.cint(), "id2")));

        assertTrue(comColsStr,
                res.commonColumns.contains(new CqlExtendedColumnName(CqlColumnType.CLUSTERING_KEY, DataType.varchar(), "id3")));

        assertTrue(comColsStr,
                res.commonColumns.contains(new CqlExtendedColumnName(CqlColumnType.REGULAR, DataType.varchar(), "deesc")));

        for (Row row : res.rows) {
            int idx = row.getInt("id2");
            assertEquals("some text-" + idx, row.getString("deesc"));
        }
    }

    @Test
    public void testExecuteSimplePkWithDynamicColumn() {

        // create data
        {
            qs.execute(new CqlQuery(CqlQueryName.USE, "USE CqlDemo"));
            for (int i = 0; i < 100; i++) {
                StringBuilder cql = new StringBuilder(
                        "INSERT INTO MyBooks (id,title,genre,publishDate,description,authors,pages,price,idx) VALUES (");
                cql.append(UUID.randomUUID()).append(",");
                cql.append("'Midnight Rain-").append(i).append("',");
                if (i == 0) {
                    cql.append("'Fantasy',");
                } else {
                    cql.append("null,");
                }
                cql.append("'2000-10-01',");

                cql.append("'Description.....-").append(i).append("',");
                cql.append("{'Ralls, Kim'},2212, {'D':2.85,'E':3.11,'F':4.22},");
                cql.append(i).append(")");
                qs.execute(new CqlQuery(CqlQueryName.INSERT, cql.toString()));
            }
        }
        CqlSelectResult res = qs.execute(new CqlQuery(CqlQueryName.SELECT, "select * from MyBooks where pages=2212"));
        assertEquals(100, res.rows.size());

        assertTrue(res.toString(),
                res.dynamicColumns.contains(new CqlExtendedColumnName(CqlColumnType.REGULAR, DataType.varchar(), "genre")));

        String comColsStr = res.commonColumns.toString();
        assertTrue(comColsStr,
                res.commonColumns.contains(new CqlExtendedColumnName(CqlColumnType.PARTITION_KEY, DataType.uuid(), "id")));

        assertTrue(comColsStr, res.commonColumns.contains(new CqlExtendedColumnName(CqlColumnType.REGULAR, DataType.set(DataType
                .varchar()), "authors")));

        assertTrue(comColsStr,
                res.commonColumns.contains(new CqlExtendedColumnName(CqlColumnType.REGULAR, DataType.cint(), "pages")));

        assertTrue(comColsStr, res.commonColumns.contains(new CqlExtendedColumnName(CqlColumnType.REGULAR, DataType.map(
                DataType.varchar(), DataType.cdouble()), "price")));

        for (Row row : res.rows) {
            int idx = row.getInt("idx");
            assertEquals("Midnight Rain-" + idx, row.getString("title"));
        }

    }
}

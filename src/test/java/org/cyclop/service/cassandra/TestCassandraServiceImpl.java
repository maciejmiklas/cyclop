package org.cyclop.service.cassandra;

import com.datastax.driver.core.DataType;
import com.datastax.driver.core.Row;
import com.google.common.collect.ImmutableSortedSet;
import java.util.UUID;
import javax.inject.Inject;
import org.cyclop.model.CqlColumnName;
import org.cyclop.model.CqlColumnType;
import org.cyclop.model.CqlExtendedColumnName;
import org.cyclop.model.CqlQuery;
import org.cyclop.model.CqlQueryName;
import org.cyclop.model.CqlSelectResult;
import org.cyclop.model.CqlTable;
import org.cyclop.test.AbstractTestCase;
import org.junit.Before;
import org.junit.Test;

import static junit.framework.Assert.assertNotNull;
import static junit.framework.Assert.assertTrue;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;

/**
 * @author Maciej Miklas
 */
public class TestCassandraServiceImpl extends AbstractTestCase {

    @Inject
    private QueryService cs;

    @Inject
    private CassandraSession cassandraSession;

    @Before
    public void setup() {
        cassandraSession.authenticate("test", "test1234");
    }

    @Test
    public void testFindColumnNames_TableDoesNotExists() {
        ImmutableSortedSet<CqlColumnName> resp = cs.findColumnNames(new CqlTable("not-existing"));
        assertNotNull(resp);
        assertEquals(0, resp.size());
    }

    @Test
    public void testFindAllColumnNames() {
        ImmutableSortedSet<CqlColumnName> allColumnNames = cs.findAllColumnNames();
        assertNotNull(allColumnNames);
        assertFalse(allColumnNames.isEmpty());

        assertTrue(allColumnNames.contains(new CqlColumnName(DataType.text(), "title")));
        assertTrue(allColumnNames.contains(new CqlColumnName(DataType.text(), "dynamicColumn1")));
        assertTrue(allColumnNames.contains(new CqlColumnName(DataType.text(), "dynamicColumn3")));
        assertTrue(allColumnNames.contains(new CqlColumnName(DataType.text(), "rpc_address")));
        assertTrue(allColumnNames.contains(new CqlColumnName(DataType.text(), "publishDate")));
    }

    @Test
    public void testExecuteCompoundPkNoDynamicColumns() {
        // create data
        {
            cs.execute(new CqlQuery(CqlQueryName.USE, "USE CqlDemo"));
            for (int i = 0; i < 50; i++) {
                StringBuilder cql = new StringBuilder("INSERT INTO CompoundTest (id,id2,id3,description) VALUES (");
                cql.append(UUID.randomUUID()).append(",");
                cql.append(i);
                cql.append(",'abc','some text-" + i + "')");
                cs.execute(new CqlQuery(CqlQueryName.INSERT, cql.toString()));
            }
        }

        CqlSelectResult res = cs.execute(new CqlQuery(CqlQueryName.SELECT, "select * from CompoundTest"));
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
                res.commonColumns.contains(new CqlExtendedColumnName(CqlColumnType.REGULAR, DataType.varchar(), "description")));

        for (Row row : res.rows) {
            int idx = row.getInt("id2");
            assertEquals("some text-" + idx, row.getString("description"));
        }
    }

    @Test
    public void testExecuteSimplePkWithDynamicColumn() {

        // create data
        {
            cs.execute(new CqlQuery(CqlQueryName.USE, "USE CqlDemo"));
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
                cs.execute(new CqlQuery(CqlQueryName.INSERT, cql.toString()));
            }
        }
        CqlSelectResult res = cs.execute(new CqlQuery(CqlQueryName.SELECT, "select * from MyBooks where pages=2212"));
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

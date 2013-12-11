package org.cyclop.service.cassandra;

import javax.inject.Inject;
import org.cyclop.TestProps;
import org.cyclop.service.model.CqlColumnName;
import org.cyclop.service.model.CqlQuery;
import org.cyclop.service.model.CqlQueryType;
import org.cyclop.service.model.CqlSelectResult;
import org.cyclop.service.model.CqlTable;
import org.junit.Ignore;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import org.springframework.test.context.web.WebAppConfiguration;

/**
 * @author Maciej Miklas
 */

@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(locations = {TestProps.TEST_CONTEXT})
@WebAppConfiguration
public class TestDefaultCassandraService {

    @Inject
    QueryService cs;

    @Ignore
    @Test
    public void testFindTableNames() {
        System.out.println(cs.findTableNamesForActiveKeySpace());
    }

    @Ignore
    @Test
    public void testFindColumnNames() {
        for (CqlTable table : cs.findTableNamesForActiveKeySpace()) {
            System.out.println(table.part + ": " + cs.findColumnNames(table));
        }
    }

    @Ignore
    @Test
    public void testFindAllColumnNames() {
        for (CqlColumnName column : cs.findAllColumnNames()) {
            System.out.println(column.part);
        }
    }

    @Ignore
    @Test
    public void testExecute() {
        cs.execute(new CqlQuery(CqlQueryType.USE, "USE CqlDemo;"));
        CqlSelectResult res = cs.execute(new CqlQuery(CqlQueryType.SELECT, "select * from MyBooks"));
        System.out.println(res);
    }

    @Ignore
    @Test
    public void testCount() {
        cs.execute(new CqlQuery(CqlQueryType.USE, "USE CqlDemo;"));
        CqlSelectResult res = cs.execute(new CqlQuery(CqlQueryType.SELECT, "select * from MyBooks"));
        System.out.println(res);
    }

}

package org.cyclop.service.cassandra;

import javax.inject.Inject;
import org.cyclop.TestProps;
import org.cyclop.model.CqlColumnName;
import org.cyclop.model.CqlQuery;
import org.cyclop.model.CqlQueryName;
import org.cyclop.model.CqlSelectResult;
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
@Ignore
public class TestCassandraServiceImpl {

    @Inject
    QueryService cs;


    @Test
    public void testFindAllColumnNames() {
        for (CqlColumnName column : cs.findAllColumnNames()) {
            System.out.println(column.part);
        }
    }

    @Test
    public void testExecute() {
        cs.execute(new CqlQuery(CqlQueryName.USE, "USE CqlDemo;"));
        CqlSelectResult res = cs.execute(new CqlQuery(CqlQueryName.SELECT, "select * from MyBooks"));
        System.out.println(res);
    }

    @Test
    public void testCount() {
        cs.execute(new CqlQuery(CqlQueryName.USE, "USE CqlDemo;"));
        CqlSelectResult res = cs.execute(new CqlQuery(CqlQueryName.SELECT, "select * from MyBooks"));
        System.out.println(res);
    }

}

package org.cyclop.service.cassandra;

import javax.inject.Inject;

import org.cyclop.AbstractTestCase;
import org.cyclop.model.CqlColumnName;
import org.cyclop.model.CqlQuery;
import org.cyclop.model.CqlQueryName;
import org.cyclop.model.CqlSelectResult;
import org.junit.Ignore;
import org.junit.Test;

/**
 * @author Maciej Miklas
 */
@Ignore
public class TestCassandraServiceImpl extends AbstractTestCase {

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

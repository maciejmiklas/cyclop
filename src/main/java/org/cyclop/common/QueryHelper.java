package org.cyclop.common;

import org.apache.commons.lang.StringUtils;
import org.cyclop.service.model.CqlKeySpace;
import org.cyclop.service.model.CqlQuery;
import org.cyclop.service.model.CqlTable;

/**
 * @author Maciej Miklas
 */
public class QueryHelper {

    public final static String KW_SELECT = "from ";

    public final static String KW_INSERT = "insert into ";

    public static CqlKeySpace extractSpace(CqlQuery query) {
        String cqlLc = query.cqlLc.replaceAll("[;]", "");
        if (!cqlLc.startsWith("use")) {
            return null;
        }

        String space = cqlLc.substring(3, cqlLc.length()).trim();
        space = StringUtils.trimToNull(space);
        if(space == null){
            return null;
        }
        return new CqlKeySpace(space);
    }

    /**
     * @param kw
     *         {@value #KW_SELECT} or {@value #KW_INSERT}
     */
    public static CqlTable extractTableName(String kw, CqlQuery query) {
        String cqlLc = query.cqlLc;
        int kwStart = cqlLc.indexOf(kw);
        if (kwStart == -1) {
            return null;
        }
        kwStart += kw.length();

        int end = cqlLc.indexOf(" ", kwStart + 1);
        if (end == -1) {
            end = cqlLc.length();
        }

        String candidate = cqlLc.substring(kwStart, end);
        candidate = StringUtils.trimToNull(candidate);
        if (candidate == null) {
            return null;
        }

        // check whether we have table with keyspace
        CqlTable result = null;
        if (candidate.contains(".") && !candidate.endsWith(".")) {
            String[] talStr = candidate.split("[.]");
            String keyspaceStr = StringUtils.trimToNull(talStr[0]);
            String tableStr = StringUtils.trimToNull(talStr[1]);
            if (tableStr == null) {
                result = null;
            } else {
                if (keyspaceStr == null) {
                    result = new CqlTable(candidate);
                } else {
                    result = new CqlTable(tableStr, keyspaceStr);
                }
            }

        } else {
            result = new CqlTable(candidate);
        }
        return result;
    }

    /**
     * @param kw
     *         {@value #KW_SELECT} or {@value #KW_INSERT}
     */
    public static CqlKeySpace extractKeyspace(String kw, CqlQuery query) {
        String cqlLc = query.cqlLc;
        int kwStart = cqlLc.indexOf(kw);
        if (kwStart == -1) {
            return null;
        }
        kwStart += kw.length();

        int end = cqlLc.indexOf(".", kwStart + 1);
        if (end == -1) {
           return null;
        }

        String candidate = cqlLc.substring(kwStart, end);
        candidate = StringUtils.trimToNull(candidate);
        if (candidate == null) {
            return null;
        }

        CqlKeySpace space = new CqlKeySpace(candidate);
        return space;
    }
}

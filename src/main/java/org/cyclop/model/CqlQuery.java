package org.cyclop.model;

import com.google.common.base.Objects;

import java.io.Serializable;

/**
 * @author Maciej Miklas
 */
public class CqlQuery implements Comparable<CqlQuery>, Serializable {

    public final String cqlLc;

    public final String cql;

    public final CqlQueryType type;

    public CqlQuery(CqlQueryType type, String cql) {
        if (cql == null || cql.isEmpty()) {
            throw new IllegalArgumentException("Empty statement");
        }
        if (type == null) {
            throw new IllegalArgumentException("Null type");
        }

        this.cqlLc = cql.trim().toLowerCase();
        this.cql = cql;
        this.type = type;
    }

    @Override
    public boolean equals(Object obj) {
        return cqlLc.equals(obj);
    }

    @Override
    public int hashCode() {
        return cqlLc.hashCode();
    }

    @Override
    public int compareTo(CqlQuery o) {
        return o.cqlLc.compareTo(cqlLc);
    }

    @Override
    public String toString() {
        return Objects.toStringHelper(this).add("cqlLc", cqlLc).add("cql", cql).add("type", type).toString();
    }

    public boolean isSingleResultType() {
        return type != CqlQueryType.SELECT;
    }
}

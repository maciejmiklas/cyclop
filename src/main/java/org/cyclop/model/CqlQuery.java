package org.cyclop.model;

import com.google.common.base.Objects;

import java.io.Serializable;

/**
 * @author Maciej Miklas
 */
public class CqlQuery implements Comparable<CqlQuery>, Serializable {

    public final String cqlLc;

    public final String cql;

    public final CqlQueryName type;

    public CqlQuery(CqlQueryName type, String cql) {
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
    public int hashCode() {
        return java.util.Objects.hash(cql);
    }

    @Override
    public boolean equals(final Object obj) {
        if (obj == null || getClass() != obj.getClass()) {
            return false;
        }

        final CqlQuery other = (CqlQuery) obj;
        return java.util.Objects.equals(cql, other.cql);
    }

    @Override
    public int compareTo(CqlQuery o) {
        return o.cqlLc.compareTo(cqlLc);
    }

    @Override
    public String toString() {
        return Objects.toStringHelper(this).add("cqlLc", cqlLc).add("cql", cql).add("type", type).toString();
    }
}

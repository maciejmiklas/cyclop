package org.cyclop.model;

import com.google.common.base.Objects;

import net.jcip.annotations.Immutable;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlTransient;
import javax.xml.bind.annotation.adapters.XmlAdapter;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;
import java.io.Serializable;

/**
 * @author Maciej Miklas
 */
@Immutable
@XmlJavaTypeAdapter(CqlQuery.Adapter.class)
// TODO extend CqlPart
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
        // TODO remove control characters
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
        return Objects.toStringHelper(this).add("cql", cql).add("type", type).toString();
    }

    @XmlRootElement
    @XmlAccessorType(XmlAccessType.FIELD)
    public final static class CqlQueryJaxb {
        public String cql;

        public CqlQueryName type;
    }

    @XmlTransient
    public final static class Adapter extends XmlAdapter<CqlQueryJaxb, CqlQuery> {

        @Override
        public CqlQuery unmarshal(CqlQueryJaxb jaxb) throws Exception {
            if (jaxb == null) {
                return null;
            }

            CqlQuery entry = new CqlQuery(jaxb.type, jaxb.cql);
            return entry;
        }

        @Override
        public CqlQueryJaxb marshal(CqlQuery histObj) throws Exception {
            if (histObj == null) {
                return null;
            }
            CqlQueryJaxb jaxb = new CqlQueryJaxb();
            jaxb.cql = histObj.cql;
            jaxb.type = histObj.type;
            return jaxb;
        }
    }
}

package org.cyclop.model;

import com.google.common.base.Objects;

import java.io.Serializable;
import javax.annotation.concurrent.Immutable;

/**
 * Cql part
 *
 * @author Maciej Miklas
 */
@Immutable
@SuppressWarnings("EQ_CHECK_FOR_OPERAND_NOT_COMPATIBLE_WITH_THIS")
public class CqlPart implements Comparable<CqlPart> {

    public final String partLc;

    public final String part;

    private CqlPart() {
        this.part = null;
        this.partLc = null;
    }

    public CqlPart(String part) {
        if (part == null || part.isEmpty()) {
            throw new IllegalArgumentException("Empty cqlPart");
        }
        // TODO remove control characters
        this.part = part;
        this.partLc = part.trim().toLowerCase();
    }

    @Override
    public boolean equals(Object obj) {
        return partLc.equals(obj);
    }

    @Override
    public int hashCode() {
        return partLc.hashCode();
    }

    @Override
    public int compareTo(CqlPart o) {
        return o.partLc.compareTo(partLc);
    }

    @Override
    public String toString() {
        return Objects.toStringHelper(this).add("partLc", partLc).add("part", part).toString();
    }

    public CqlType type() {
        return CqlType.PART;
    }
}

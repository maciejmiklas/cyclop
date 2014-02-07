package org.cyclop.model;

import net.jcip.annotations.Immutable;

import com.google.common.base.Objects;

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
	this.part = part.replaceAll("\\p{Cc}", "");
	this.partLc = this.part.trim().toLowerCase();
    }

    @Override
    public int compareTo(CqlPart o) {
	return o.partLc.compareTo(partLc);
    }

    @edu.umd.cs.findbugs.annotations.SuppressWarnings("EQ_CHECK_FOR_OPERAND_NOT_COMPATIBLE_WITH_THIS")
    @Override
    public boolean equals(Object obj) {
	if (!(obj instanceof CqlPart)) {
	    return false;
	}
	CqlPart cqlObj = (CqlPart) obj;
	return partLc.equals(cqlObj.partLc);
    }

    @Override
    public int hashCode() {
	return partLc.hashCode();
    }

    public String toDisplayString() {
	return part;
    }

    @Override
    public String toString() {
	return Objects.toStringHelper(this).add("partLc", partLc).add("part", part).toString();
    }

    public CqlType type() {
	return CqlType.PART;
    }
}

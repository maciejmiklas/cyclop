package org.cyclop.model;

import net.jcip.annotations.Immutable;

/** @author Maciej Miklas */
@Immutable
public final class CqlIndex extends CqlPart {

	public CqlIndex(String part) {
		super(part);
	}

	@Override
	public String toString() {
		return "CqlIndex{" + "part='" + part + '\'' + '}';
	}

	@Override
	public CqlType type() {
		return CqlType.INDEX;
	}

	// TODO formatter should add enter after this annotation
	@edu.umd.cs.findbugs.annotations.SuppressWarnings("EQ_CHECK_FOR_OPERAND_NOT_COMPATIBLE_WITH_THIS") @Override
	public boolean equals(Object obj) {
		if (obj == null || getClass() != obj.getClass()) {
			return false;
		}
		CqlPart cqlObj = (CqlPart) obj;
		return partLc.equals(cqlObj.partLc);
	}

	@Override
	public int hashCode() {
		return partLc.hashCode();
	}
}

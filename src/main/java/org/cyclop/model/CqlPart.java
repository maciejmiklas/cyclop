package org.cyclop.model;

import com.google.common.base.Objects;
import net.jcip.annotations.Immutable;

import org.apache.commons.lang.StringEscapeUtils;
import org.hibernate.validator.constraints.NotEmpty;

import java.io.Serializable;

/** @author Maciej Miklas */
@Immutable
@SuppressWarnings("EQ_CHECK_FOR_OPERAND_NOT_COMPATIBLE_WITH_THIS")
public class CqlPart implements Comparable<CqlPart>, Serializable, DisplaySupport {

	@NotEmpty
	public final String partLc;

	@NotEmpty
	public final String part;

	public CqlPart(String part) {
		if (part != null) {
			this.part = part.replaceAll("\\p{Cc}", "");
			this.partLc = this.part.trim().toLowerCase();
		} else {
			this.part = null;
			this.partLc = null;
		}
	}

	@Override
	public int compareTo(CqlPart o) {
		return o.partLc.compareTo(partLc);
	}

	@edu.umd.cs.findbugs.annotations.SuppressWarnings("EQ_CHECK_FOR_OPERAND_NOT_COMPATIBLE_WITH_THIS")
	@Override
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

	public String toDisplayString() {
		return StringEscapeUtils.escapeHtml(part);
	}

	@Override
	public String toString() {
		return Objects.toStringHelper(this).add("partLc", partLc).add("part", part).toString();
	}

	public CqlType type() {
		return CqlType.PART;
	}
}

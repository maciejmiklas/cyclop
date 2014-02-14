package org.cyclop.model;

import com.google.common.base.Objects;
import net.jcip.annotations.Immutable;

/**
 * Cql keyword
 *
 * @author Maciej Miklas
 */
@Immutable
public final class CqlTable extends CqlPart {

	/** can be null */
	public final CqlKeySpace keySpace;

	public CqlTable(String keySpace, String table) {
		super(table);

		if (keySpace == null) {
			this.keySpace = null;
		} else {
			this.keySpace = new CqlKeySpace(keySpace);
		}
	}


	@Override
	public int hashCode() {
		return java.util.Objects.hash(partLc, keySpace);
	}

	@Override
	public boolean equals(Object obj) {
		if (obj == null || getClass() != obj.getClass()) {
			return false;
		}
		final CqlTable other = (CqlTable) obj;
		return java.util.Objects.equals(partLc, other.partLc) && java.util.Objects.equals(keySpace, other.keySpace);
	}

	public CqlTable(String table) {
		this(null, table);
	}

	@Override
	public int compareTo(CqlPart o) {
		if (o == null || getClass() != o.getClass()) {
			return -1;
		}
		CqlTable table = (CqlTable) o;

		return toDisplayString().toLowerCase().compareTo(table.toDisplayString().toLowerCase());
	}

	public String toDisplayString() {
		StringBuilder buf = new StringBuilder();
		if (keySpace != null) {
			buf.append(keySpace.part).append(".");
		}
		buf.append(part);
		return buf.toString();
	}

	@Override
	public String toString() {
		return Objects.toStringHelper(this).add("table", part).add("keySpace", keySpace).toString();
	}

	@Override
	public CqlType type() {
		return CqlType.TABLE;
	}
}

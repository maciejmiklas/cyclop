package org.cyclop.model;

import com.datastax.driver.core.DataType;
import net.jcip.annotations.Immutable;

import java.io.NotSerializableException;
import java.util.Objects;

/** @author Maciej Miklas */
@Immutable
public class CqlColumnName extends CqlPart {

	public transient final DataType dataType;

	public CqlColumnName(DataType dataType, String columnName) {
		super(columnName);
		if (dataType == null) {
			throw new IllegalArgumentException("Null DataType");
		}
		this.dataType = dataType;
	}

	public CqlColumnName(String columnName) {
		this(DataType.text(), columnName);
	}

	@Override
	public boolean equals(Object obj) {
		if (obj == null || getClass() != obj.getClass()) {
			return false;
		}
		final CqlColumnName other = (CqlColumnName) obj;
		return Objects.equals(partLc, other.partLc) && Objects.equals(dataType, other.dataType);
	}

	@Override
	public int hashCode() {
		return Objects.hash(partLc, dataType);
	}

	@Override
	public String toString() {
		return com.google.common.base.Objects.toStringHelper(this).add("part", part).add("dataType", dataType)
				.toString();
	}

	@Override
	public CqlType type() {
		return CqlType.COLUMN;
	}

	private void writeObject(java.io.ObjectOutputStream out) throws NotSerializableException {
		throw new NotSerializableException("Serialization not supported due to non-serializable field: DataType");
	}
}

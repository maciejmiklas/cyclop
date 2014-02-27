package org.cyclop.model;

import net.jcip.annotations.Immutable;
import org.cyclop.validation.BeanValidator;

import java.util.Objects;

/** @author Maciej Miklas */
@Immutable
public final class CqlPartitionKey extends CqlExtendedColumnName {

	protected CqlPartitionKey(CqlDataType dataType, String columnName) {
		super(CqlColumnType.PARTITION_KEY, dataType, columnName);
		BeanValidator.create(this).validate();
	}

	public static CqlPartitionKey fromColumn(CqlExtendedColumnName col) {
		return new CqlPartitionKey(col.dataType, col.part);
	}

	@Override
	public String toString() {
		return com.google.common.base.Objects.toStringHelper(this).add("columnType", columnType).add("part", part)
				.add("dataType", dataType).toString();
	}

	@Override
	public int hashCode() {
		return Objects.hash(partLc, columnType, dataType);
	}

	@Override
	public boolean equals(Object obj) {
		if (obj == null || getClass() != obj.getClass()) {
			return false;
		}
		final CqlExtendedColumnName other = (CqlExtendedColumnName) obj;
		return Objects.equals(partLc, other.partLc) && Objects.equals(columnType, other.columnType) &&
				Objects.equals(dataType, other.dataType);
	}
}

package org.cyclop.model;

import com.google.common.base.Objects;
import net.jcip.annotations.Immutable;

import javax.validation.Valid;
import javax.validation.constraints.NotNull;
import java.io.Serializable;

/** @author Maciej Miklas */
@Immutable
public class CqlColumnValue implements Serializable {

	@NotNull
	public final Class<?> valueClass;

	@NotNull
	public final Object value;

	@NotNull
	@Valid
	public final CqlExtendedColumnName columnName;

	public CqlColumnValue(Class<?> valueClass, Object value, CqlExtendedColumnName columnName) {
		this.valueClass = valueClass;
		this.value = value;
		this.columnName = columnName;
	}

	@Override
	public String toString() {
		return Objects.toStringHelper(this).add("valueClass", valueClass).add("value", value)
				.add("columnName", columnName).toString();
	}

	@Override
	public int hashCode() {
		return java.util.Objects.hash(valueClass, value, columnName);
	}

	@Override
	public boolean equals(Object obj) {
		if (obj == null || getClass() != obj.getClass()) {
			return false;
		}
		final CqlColumnValue other = (CqlColumnValue) obj;
		return java.util.Objects.equals(valueClass, other.valueClass) && java.util.Objects.equals(value, other.value) &&
				java.util.Objects.equals(columnName, other.columnName);
	}
}

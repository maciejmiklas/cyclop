package org.cyclop.model;

import com.google.common.base.Objects;
import net.jcip.annotations.Immutable;

import java.io.Serializable;

/** @author Maciej Miklas */
@Immutable
public class CqlColumnValue implements Serializable {

	public final Class<?> valueClass;

	public final Object value;

	public final CqlExtendedColumnName columnName;

	public CqlColumnValue(Class<?> valueClass, Object value, CqlExtendedColumnName columnName) {
		this.valueClass = valueClass;
		this.value = value;
		this.columnName = columnName;
	}

	@Override
	public String toString() {
		return Objects.toStringHelper(this).add("valueClass", valueClass).add("prefix", value)
				.add("columnName", columnName).toString();
	}
}

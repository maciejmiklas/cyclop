package org.cyclop.model;

import com.google.common.base.Objects;
import net.jcip.annotations.Immutable;

import javax.validation.Valid;
import javax.validation.constraints.NotNull;

/** @author Maciej Miklas */
@Immutable
public final class CqlPartitionKeyValue extends CqlColumnValue {

	@NotNull
	@Valid
	public final CqlPartitionKey cqlPartitionKey;

	public CqlPartitionKeyValue(Class<?> valueClass, Object value, CqlPartitionKey cqlPartitionKey) {
		super(valueClass, value, cqlPartitionKey);
		this.cqlPartitionKey = cqlPartitionKey;
	}

	@Override
	public String toString() {
		return Objects.toStringHelper(this).add("valueClass", valueClass).add("prefix", value)
				.add("cqlPartitionKey", cqlPartitionKey).toString();
	}
}

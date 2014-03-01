package org.cyclop.model;

import net.jcip.annotations.Immutable;

import javax.validation.Valid;
import javax.validation.constraints.NotNull;
import java.io.Serializable;
import java.util.Objects;

/** @author Maciej Miklas */
@Immutable
public final class ContextCqlCompletion implements Serializable {
	@NotNull @Valid
	public final CqlQueryName queryName;

	@NotNull @Valid
	public final CqlCompletion cqlCompletion;

	public ContextCqlCompletion(CqlQueryName queryName, CqlCompletion cqlCompletion) {
		this.queryName = queryName;
		this.cqlCompletion = cqlCompletion;
	}

	@Override
	public int hashCode() {
		return Objects.hash(queryName, cqlCompletion);
	}

	@Override
	public boolean equals(Object obj) {
		if (obj == null || getClass() != obj.getClass()) {
			return false;
		}
		final ContextCqlCompletion other = (ContextCqlCompletion) obj;
		return Objects.equals(queryName, other.queryName) && Objects.equals(cqlCompletion, other.cqlCompletion);
	}

	@Override
	public String toString() {
		return com.google.common.base.Objects.toStringHelper(this).add("queryName", queryName)
				.add("cqlCompletion", cqlCompletion).toString();
	}
}

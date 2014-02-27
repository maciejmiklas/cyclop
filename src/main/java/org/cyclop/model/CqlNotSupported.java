package org.cyclop.model;

import net.jcip.annotations.Immutable;
import org.cyclop.validation.BeanValidator;

/** @author Maciej Miklas */
@Immutable
public final class CqlNotSupported extends CqlKeyword {

	public CqlNotSupported(String part) {
		super(part);
		BeanValidator.create(this).validate();
	}

	@Override
	public String toString() {
		return "CqlKeyword{" + "part='" + part + '\'' + '}';
	}

	@Override
	public CqlType type() {
		return CqlType.NOT_SUPPORTED;
	}
}

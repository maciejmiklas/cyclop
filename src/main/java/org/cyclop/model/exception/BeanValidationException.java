package org.cyclop.model.exception;

import javax.validation.ConstraintViolation;
import java.util.Map;
import java.util.Set;

/** @author Maciej Miklas */
public final class BeanValidationException extends IllegalArgumentException {

	private Map<String, Set<ConstraintViolation<Object>>> violations;

	public BeanValidationException(Map<String, Set<ConstraintViolation<Object>>> violations) {
		super("Constraint violation: " + violations);
		this.violations = violations;
	}

	public Map<String, Set<ConstraintViolation<Object>>> getViolations() {
		return violations;
	}

}

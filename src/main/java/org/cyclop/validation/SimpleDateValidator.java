package org.cyclop.validation;

import javax.validation.ConstraintValidator;
import javax.validation.ConstraintValidatorContext;
import java.text.SimpleDateFormat;

/** @author Maciej Miklas */
public class SimpleDateValidator implements ConstraintValidator<SimpleDate, String> {

	public void initialize(SimpleDate annotation) {
	}

	@Override
	public boolean isValid(String value, ConstraintValidatorContext context) {
		if (value == null) {
			return false;
		}

		try {
			new SimpleDateFormat(value);
		} catch (IllegalArgumentException e) {
			return false;
		}

		return true;
	}
}

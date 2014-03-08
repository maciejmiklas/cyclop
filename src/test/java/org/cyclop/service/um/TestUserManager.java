package org.cyclop.service.um;

import org.cyclop.model.UserIdentifier;
import org.cyclop.model.exception.BeanValidationException;
import org.cyclop.test.AbstractTestCase;
import org.junit.Test;

import javax.inject.Inject;

/** @author Maciej Miklas */
public class TestUserManager extends AbstractTestCase {

	@Inject
	private UserManager um;

	@Test(expected = BeanValidationException.class)
	public void testRegisterIdentifier_Validation() {
		um.storeIdentifier(new UserIdentifier(null));
	}

	@Test(expected = BeanValidationException.class)
	public void testStorePreferences_Validation() {
		um.storePreferences(null);
	}

}

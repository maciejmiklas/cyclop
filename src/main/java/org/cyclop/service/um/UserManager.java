package org.cyclop.service.um;

import org.cyclop.model.UserIdentifier;
import org.cyclop.model.UserPreferences;

import javax.validation.constraints.NotNull;

/** @author Maciej Miklas */
public interface UserManager {

	void registerIdentifier(@NotNull UserIdentifier id);

	@NotNull
	UserIdentifier readIdentifier();

	void storePreferences(@NotNull UserPreferences preferences);

	@NotNull
	UserPreferences readPreferences();
}

package org.cyclop.service.um;

import org.cyclop.model.UserIdentifier;
import org.cyclop.model.UserPreferences;

/**
 * @author Maciej Miklas
 */
public interface UserManager {

    void registerIdentifier(UserIdentifier id);

    UserIdentifier readIdentifier();

    void storePreferences(UserPreferences preferences);

    UserPreferences readPreferences();
}

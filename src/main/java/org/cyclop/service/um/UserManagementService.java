package org.cyclop.service.um;

import org.cyclop.model.UserIdentifier;
import org.cyclop.model.UserPreferences;

/**
 * @author Maciej Miklas
 */
public interface UserManagementService {

    void storeIdentifier(UserIdentifier id);

    UserIdentifier readIdentifier();

    void storePreferences(UserPreferences preferences);

    UserPreferences readPreferences();
}

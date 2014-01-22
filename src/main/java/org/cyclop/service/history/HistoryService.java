package org.cyclop.service.history;

import java.util.List;

import org.cyclop.model.QueryHistoryEntry;
import org.cyclop.model.UserIdentifier;

/**
 * HTTP session scoped history service.
 * 
 * @author Maciej Miklas
 */
public interface HistoryService {

    /**
     * @return null if history has been initialized for given user id, or when given user id is already connected to
     *         existing history. If history already exists for some user id, and given user id differs from it, then
     *         given user id will be rejected and method will return user id of existing history. This means that it's
     *         not possible to re-initialize existing history for another user id.
     */
    UserIdentifier initialize(UserIdentifier userId);

    /**
     * @return null if it has not been initialized yet
     */
    UserIdentifier getCurrentIdentifier();

    void submitLastQuery(QueryHistoryEntry entry);

    void assignToStarred(QueryHistoryEntry entry);

    List<QueryHistoryEntry> readHistory();

    List<QueryHistoryEntry> readStarred();

    boolean supported();
}

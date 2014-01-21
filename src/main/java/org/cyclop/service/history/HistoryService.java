package org.cyclop.service.history;

import java.util.List;

import com.google.common.collect.ImmutableList;
import org.cyclop.model.CqlQuery;
import org.cyclop.model.QueryHistoryEntry;
import org.cyclop.model.ServiceException;
import org.cyclop.model.UserIdentifier;

/**
 * @author Maciej Miklas
 */
public interface HistoryService {
    
    // TODO support time zone from browser
    void submitLastQuery(CqlQuery query, UserIdentifier userId)throws ServiceException;
    
    ImmutableList<QueryHistoryEntry> readHistory(UserIdentifier userId)throws ServiceException;

    boolean supported();
}

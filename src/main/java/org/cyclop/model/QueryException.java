package org.cyclop.model;

import javax.annotation.concurrent.Immutable;

/**
 * @author Maciej Miklas
 */
@Immutable
public final class QueryException extends ServiceException {
    public QueryException(String message) {
        super(message);
    }

    public QueryException(String message, Exception cause) {
        super(message, cause);
    }
}

package org.cyclop.model.exception;

import net.jcip.annotations.Immutable;

/** @author Maciej Miklas */
@Immutable
public final class QueryException extends ServiceException {
	public QueryException(String message) {
		super(message);
	}

	public QueryException(String message, Exception cause) {
		super(message, cause);
	}
}

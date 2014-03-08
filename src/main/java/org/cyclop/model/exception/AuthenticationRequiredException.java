package org.cyclop.model.exception;

public class AuthenticationRequiredException extends RuntimeException {
	public AuthenticationRequiredException(String message) {
		super(message);
	}

	public AuthenticationRequiredException(String message, Exception cause) {
		super(message, cause);
	}
}

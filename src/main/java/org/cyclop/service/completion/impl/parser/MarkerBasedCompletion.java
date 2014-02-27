package org.cyclop.service.completion.impl.parser;

import org.cyclop.model.CqlPart;

/** @author Maciej Miklas */

public abstract class MarkerBasedCompletion implements CqlPartCompletion {

	private CqlPart startMarker = null;

	protected MarkerBasedCompletion(CqlPart startMarker) {
		if (startMarker == null) {
			throw new IllegalArgumentException("NUll start marker");
		}
		this.startMarker = startMarker;

	}

	public final CqlPart startMarker() {
		return startMarker;
	}
}

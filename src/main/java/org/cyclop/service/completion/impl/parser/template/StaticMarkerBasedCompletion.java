package org.cyclop.service.completion.impl.parser.template;

import org.cyclop.model.CqlCompletion;
import org.cyclop.model.CqlPart;
import org.cyclop.model.CqlQuery;
import org.cyclop.service.completion.impl.parser.MarkerBasedCompletion;

/** @author Maciej Miklas */
public abstract class StaticMarkerBasedCompletion extends MarkerBasedCompletion {

	private CqlCompletion completion;

	protected StaticMarkerBasedCompletion(CqlPart startMarker, CqlCompletion completion) {
		super(startMarker);
		this.completion = completion;
	}

	@Override
	public final CqlCompletion getCompletion(CqlQuery query) {
		return completion;
	}
}

package org.cyclop.web.panels.queryeditor.editor;

import org.apache.wicket.Component;
import org.cyclop.model.ContextCqlCompletion;

import java.io.Serializable;

/** @author Maciej Miklas */
public interface CompletionChangeListener extends Serializable {

	void onCompletionChange(ContextCqlCompletion currentCompletion);

	Component getReferencesForRefresh();
}

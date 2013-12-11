package org.cyclop.web.pages.cqlcommander.editor;

import java.io.Serializable;
import org.apache.wicket.Component;
import org.cyclop.service.model.ContextCqlCompletion;

/**
 * @author Maciej Miklas
 */
public interface CompletionChangeListener extends Serializable {

    void onCompletionChange(ContextCqlCompletion currentCompletion);

    Component getReferencesForRefresh();
}

package org.cyclop.service.importer;

import javax.validation.constraints.NotNull;
import java.io.InputStream;

/** @author Maciej Miklas */
public interface QueryImporter {

	void importScript(@NotNull InputStream input, @NotNull ResultWritter resultWritter, boolean updateHistory,
					  boolean continueWithErrors);
}

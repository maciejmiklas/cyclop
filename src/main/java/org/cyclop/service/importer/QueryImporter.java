package org.cyclop.service.importer;

import org.cyclop.service.importer.model.ImportStats;

import javax.validation.constraints.NotNull;
import java.io.InputStream;


/** @author Maciej Miklas */
public interface QueryImporter {

	@NotNull
	ImportStats importScript(@NotNull InputStream input, @NotNull ResultWritter resultWritter, boolean updateHistory,
							 boolean continueWithErrors);
}

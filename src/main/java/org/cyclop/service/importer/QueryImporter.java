package org.cyclop.service.importer;

import java.io.InputStream;

import javax.validation.constraints.NotNull;

import org.cyclop.service.importer.model.ImportStats;


/** @author Maciej Miklas */
public interface QueryImporter {

    @NotNull ImportStats importScript(
	    @NotNull InputStream input,
	    @NotNull ResultWritter resultWritter,
	    boolean updateHistory,
	    boolean continueWithErrors);
}

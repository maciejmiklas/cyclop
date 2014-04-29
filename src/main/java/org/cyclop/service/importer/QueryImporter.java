package org.cyclop.service.importer;

import org.cyclop.service.importer.model.ImportConfig;
import org.cyclop.service.importer.model.ImportStats;

import javax.validation.constraints.NotNull;
import java.io.InputStream;


/** @author Maciej Miklas */
public interface QueryImporter {

	String IMPL_SERIAL = "SerialQueryImporter";

	String IMPL_PARALLEL = "ParallelQueryImporter";

	@NotNull
	ImportStats importScript(@NotNull InputStream input, @NotNull ResultWriter resultWriter, ImportConfig config);
}

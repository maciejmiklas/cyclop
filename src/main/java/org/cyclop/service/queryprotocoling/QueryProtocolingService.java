package org.cyclop.service.queryprotocoling;

import javax.validation.constraints.NotNull;

/** @author Maciej Miklas */
public interface QueryProtocolingService<H> {

	void store(@NotNull H history);

	/** @return never null, creates empty if not found on disk */
	@NotNull
	H read();

	boolean supported();
}

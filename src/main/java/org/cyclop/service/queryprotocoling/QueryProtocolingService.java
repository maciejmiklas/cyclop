package org.cyclop.service.queryprotocoling;

/** @author Maciej Miklas */
public interface QueryProtocolingService<H> {

	void store(H history);

	/** @return never null, creates empty if not found on disk */
	H readHistory();

	boolean supported();
}

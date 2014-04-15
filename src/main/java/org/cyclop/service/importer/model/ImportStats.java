package org.cyclop.service.importer.model;

import net.jcip.annotations.Immutable;
import org.apache.commons.lang3.time.StopWatch;

import java.io.Serializable;

@Immutable
public class ImportStats implements Serializable {

	public transient final StopWatch runtime;

	public final int successCount;

	public final int errorCount;

	public ImportStats(StopWatch runtime, int successCount, int errorCount) {
		this.runtime = runtime;
		this.successCount = successCount;
		this.errorCount = errorCount;
	}

	@Override
	public String toString() {
		return "ImportResult [runtime=" + runtime + ", successCount=" + successCount + ", errorCount=" + errorCount +
				"]";
	}

}

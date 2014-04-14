package org.cyclop.service.importer.model;

import java.io.Serializable;

import net.jcip.annotations.Immutable;

import org.apache.commons.lang3.time.StopWatch;

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
	return "ImportResult [runtime="
		+ runtime
		+ ", successCount="
		+ successCount
		+ ", errorCount="
		+ errorCount
		+ "]";
    }

}

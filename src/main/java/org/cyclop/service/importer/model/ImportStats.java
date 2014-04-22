package org.cyclop.service.importer.model;

import com.google.common.base.Objects;
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
		return Objects.toStringHelper(this).add("runtime", runtime).add("successCount", successCount)
				.add("errorCount", errorCount).toString();
	}
}

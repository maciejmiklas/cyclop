package org.cyclop.service.importer.impl;

import com.google.common.base.Objects;
import net.jcip.annotations.ThreadSafe;

import java.util.concurrent.atomic.AtomicInteger;

/** @author Maciej Miklas */
@ThreadSafe
final class StatsCollector {

	public final AtomicInteger error = new AtomicInteger(0);

	public final AtomicInteger success = new AtomicInteger(0);

	@Override
	public String toString() {
		return Objects.toStringHelper(this).add("error", error).add("success", success).toString();
	}
}

/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.cyclop.model;

import java.io.Serializable;
import java.util.Collection;
import java.util.Objects;

import javax.validation.Valid;
import javax.validation.constraints.NotNull;

import net.jcip.annotations.Immutable;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.common.collect.ImmutableSortedSet;

/** @author Maciej Miklas */
@Immutable
public final class CqlCompletion implements Serializable {
    private final static Logger LOG = LoggerFactory.getLogger(CqlCompletion.class);

    private final static String[] VALUE_PREF = { "'", "(", ",", ":" };

    /**
     * used during typing, contains all possible combinations that will be
     * suggested when pressing TAB
     */
    @NotNull
    @Valid
    public final ImmutableSortedSet<? extends CqlPart> fullCompletion;

    /** used for hint window */
    @NotNull
    @Valid
    public final ImmutableSortedSet<? extends CqlPart> minCompletion;

    private CqlCompletion(
	    ImmutableSortedSet<? extends CqlPart> fullCompletion,
	    ImmutableSortedSet<? extends CqlPart> minCompletion) {
	this.fullCompletion = fullCompletion;
	this.minCompletion = minCompletion;
    }

    private CqlCompletion() {
	this.fullCompletion = ImmutableSortedSet.of();
	this.minCompletion = ImmutableSortedSet.of();
    }

    @Override
    public String toString() {
	return "CqlCompletion{"
		+ "fullCompletion="
		+ fullCompletion
		+ ", minCompletion="
		+ minCompletion
		+ '}';
    }

    @Override
    public int hashCode() {
	return Objects.hash(fullCompletion);
    }

    @Override
    public boolean equals(Object obj) {
	if (obj == null || getClass() != obj.getClass()) {
	    return false;
	}
	final CqlCompletion other = (CqlCompletion) obj;
	return Objects.equals(fullCompletion, other.fullCompletion);
    }

    public boolean isEmpty() {
	return fullCompletion.isEmpty();
    }

    /** @author Maciej Miklas */
    public static class Builder {

	private final static CqlCompletion EMPTY = new CqlCompletion();

	private ImmutableSortedSet.Builder<CqlPart> full = ImmutableSortedSet.naturalOrder();

	private ImmutableSortedSet.Builder<CqlPart> min = ImmutableSortedSet.naturalOrder();

	public static Builder naturalOrder() {
	    return new Builder();
	}

	public static CqlCompletion empty() {
	    return EMPTY;
	}

	public Builder full(Collection<? extends CqlPart> part) {
	    full.addAll(part);
	    return this;
	}

	public Builder min(Collection<? extends CqlPart> part) {
	    min.addAll(part);
	    return this;
	}

	public Builder all(Collection<? extends CqlPart> part) {
	    min.addAll(part);
	    full.addAll(part);
	    return this;
	}

	public Builder full(CqlPart part) {
	    full.add(part);
	    return this;
	}

	public Builder min(CqlPart part) {
	    min.add(part);
	    return this;
	}

	public Builder all(CqlPart part) {
	    min.add(part);
	    full.add(part);
	    return this;
	}

	public Builder prefix(CqlPart part) {
	    for (String pref : VALUE_PREF) {
		prefix(pref, part);
	    }
	    return this;
	}

	private Builder prefix(String prefix, CqlPart part) {
	    min.add(part);
	    full.add(part);

	    CqlPart prefixPart = new CqlPart(prefix + part.toDisplayString());
	    full.add(prefixPart);
	    return this;
	}

	private Builder prefix(String prefix, Collection<? extends CqlPart> col) {
	    for (CqlPart part : col) {
		prefix(prefix, part);
	    }
	    return this;
	}

	public Builder value(Collection<? extends CqlPart> col) {
	    for (String pref : VALUE_PREF) {
		prefix(pref, col);
	    }
	    return this;
	}

	public BuilderTemplate template() {
	    return new BuilderTemplate(min.build(), full.build());
	}

	public CqlCompletion build() {
	    CqlCompletion comp = new CqlCompletion(full.build(), min.build());
	    LOG.trace("Build completion: {}", comp);
	    return comp;
	}

    }

    public final static class BuilderTemplate {
	private ImmutableSortedSet<CqlPart> full;

	private ImmutableSortedSet<CqlPart> min;

	public BuilderTemplate(ImmutableSortedSet<CqlPart> min, ImmutableSortedSet<CqlPart> full) {
	    this.full = full;
	    this.min = min;
	}

	public Builder naturalOrder() {
	    return Builder.naturalOrder().full(full).min(min);
	}

	@Override
	public String toString() {
	    return "BuilderTemplate{" + "full=" + full + ", min=" + min + '}';
	}
    }
}

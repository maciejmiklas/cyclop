package org.cyclop.model;

import com.google.common.collect.ImmutableSortedSet;

import java.io.Serializable;
import java.util.Collection;
import java.util.Objects;

/**
 * @author Maciej Miklas
 */
public class CqlCompletion implements Serializable {

    /**
     * used during typing, contains all possible combinations that will be suggested when pressing TAB
     */
    public final ImmutableSortedSet<? extends CqlPart> fullCompletion;

    /**
     * used for hint window
     */
    public final ImmutableSortedSet<? extends CqlPart> minCompletion;


    private CqlCompletion(ImmutableSortedSet<? extends CqlPart> fullCompletion, ImmutableSortedSet<? extends CqlPart> minCompletion) {
        if (minCompletion == null) {
            throw new IllegalArgumentException("Null minCompletion");
        }

        if (fullCompletion == null) {
            throw new IllegalArgumentException("Null fullCompletion");
        }

        this.fullCompletion = fullCompletion;
        this.minCompletion = minCompletion;
    }

    @Override
    public String toString() {
        return "CqlCompletion{" + "fullCompletion=" + fullCompletion + ", minCompletion=" + minCompletion + '}';
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

    /**
     * @author Maciej Miklas
     */
    public static class Builder {

        private ImmutableSortedSet.Builder<CqlPart> full = ImmutableSortedSet.naturalOrder();
        private ImmutableSortedSet.Builder<CqlPart> min = ImmutableSortedSet.naturalOrder();

        public static Builder naturalOrder() {
            return new Builder();
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

        public BuilderTemplate template() {
            return new BuilderTemplate(min.build(), full.build());
        }

        public CqlCompletion build() {
            return new CqlCompletion(full.build(), min.build());
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
            return "BuilderTemplate{" +
                    "full=" + full +
                    ", min=" + min +
                    '}';
        }
    }
}

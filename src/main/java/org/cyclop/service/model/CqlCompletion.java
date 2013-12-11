package org.cyclop.service.model;

import com.google.common.collect.ImmutableSortedSet;
import java.io.Serializable;
import java.util.Objects;

/**
 * @author Maciej Miklas
 */
public class CqlCompletion implements Serializable {

    /** used during typing, contains all possible combinations that will be suggested when pressing TAB */
    public final ImmutableSortedSet<? extends CqlPart> fullCompletion;

    /** used for hint window */
    public final ImmutableSortedSet<? extends CqlPart> minCompletion;


    public CqlCompletion(ImmutableSortedSet<? extends CqlPart> fullCompletion, ImmutableSortedSet<? extends CqlPart> minCompletion) {
        if (minCompletion == null) {
            throw new IllegalArgumentException("Null minCompletion");
        }

        if (fullCompletion == null) {
            throw new IllegalArgumentException("Null fullCompletion");
        }

        this.fullCompletion = fullCompletion;
        this.minCompletion = minCompletion;
    }

    public CqlCompletion() {
        this.fullCompletion = ImmutableSortedSet.of();
        this.minCompletion =ImmutableSortedSet.of();
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
}
